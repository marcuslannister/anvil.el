;;; anvil-org-index.el --- Persistent SQLite index for org files -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Fujisawa Electric Management Office

;; This file is part of anvil.el.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Persistent SQLite-backed index of headlines, tags, properties and
;; IDs across configured org directories.  Intended to let MCP read
;; tools avoid re-parsing large org files on every call.
;;
;; See docs/design/02-org-index.org for the full design.  This file
;; covers Phase 1 only: schema creation, full rebuild, status, and a
;; pure-regexp scanner.  Filesystem watcher, incremental refresh and
;; MCP tool integration come in later phases.
;;
;; Backend selection at load time:
;;   1. Emacs 29+ with `sqlite-available-p' -> built-in sqlite
;;   2. `emacsql' installed                  -> emacsql (TODO, Phase 1+)
;;   3. otherwise                             -> `user-error'
;;
;; Only the built-in backend is fully implemented in Phase 1.  The
;; emacsql path is sketched but signals a clear error until completed.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

;;; Customization

(defgroup anvil-org-index nil
  "Anvil persistent org index."
  :group 'anvil
  :prefix "anvil-org-index-")

(defcustom anvil-org-index-db-path
  (expand-file-name "anvil-org-index.db" user-emacs-directory)
  "Path to the SQLite database file."
  :type 'file
  :group 'anvil-org-index)

(defcustom anvil-org-index-paths nil
  "List of directories to recursively index.
Each entry is a directory path.  Non-existent entries are skipped
with a warning."
  :type '(repeat directory)
  :group 'anvil-org-index)

(defcustom anvil-org-index-exclude-patterns
  '("/\\.git/" "/archive/" "/\\.cache/")
  "Regex patterns for paths to exclude from indexing."
  :type '(repeat regexp)
  :group 'anvil-org-index)

(defcustom anvil-org-index-todo-keywords
  '("TODO" "NEXT" "WAIT" "PROJECT" "SOMEDAY"
    "DONE" "CANCEL" "CANCELED" "CANCELLED"
    "NOTE" "MEMO")
  "TODO keywords recognized by the regex scanner.
Any leading uppercase word on a headline that matches this list
is treated as the TODO state."
  :type '(repeat string)
  :group 'anvil-org-index)

(defconst anvil-org-index-schema-version 1
  "Current schema version.  Bump on incompatible changes.")

;;; Backend

(defvar anvil-org-index--backend nil
  "Active backend symbol: `builtin' or `emacsql'.
Populated lazily by `anvil-org-index--detect-backend'.")

(defvar anvil-org-index--db nil
  "Open database handle, or nil.")

(defun anvil-org-index--detect-backend ()
  "Return the best available backend symbol, or signal `user-error'."
  (cond
   ((and (fboundp 'sqlite-available-p) (sqlite-available-p))
    'builtin)
   ((require 'emacsql nil t)
    'emacsql)
   (t
    (user-error
     "anvil-org-index: neither built-in sqlite (Emacs 29+) nor emacsql is available"))))

(defun anvil-org-index--open (path)
  "Open database at PATH using the active backend."
  (pcase anvil-org-index--backend
    ('builtin (sqlite-open path))
    ('emacsql
     (user-error "anvil-org-index: emacsql backend not yet implemented"))))

(defun anvil-org-index--close (db)
  "Close DB."
  (pcase anvil-org-index--backend
    ('builtin (when (sqlitep db) (sqlite-close db)))
    ('emacsql nil)))

(defun anvil-org-index--execute (db sql &optional params)
  "Run SQL (DDL or INSERT/UPDATE/DELETE) against DB with PARAMS."
  (pcase anvil-org-index--backend
    ('builtin (sqlite-execute db sql params))
    ('emacsql (user-error "anvil-org-index: emacsql backend not yet implemented"))))

(defun anvil-org-index--select (db sql &optional params)
  "Run SELECT SQL against DB with PARAMS.  Returns list of rows."
  (pcase anvil-org-index--backend
    ('builtin (sqlite-select db sql params))
    ('emacsql (user-error "anvil-org-index: emacsql backend not yet implemented"))))

(defmacro anvil-org-index--with-transaction (db &rest body)
  "Run BODY inside a transaction on DB.
Uses raw BEGIN/COMMIT/ROLLBACK for portability across backends
and Emacs versions (avoids `with-sqlite-transaction' which is not
present on all targets)."
  (declare (indent 1) (debug t))
  (let ((db-sym (make-symbol "db")))
    `(let ((,db-sym ,db))
       (anvil-org-index--execute ,db-sym "BEGIN")
       (condition-case err
           (prog1 (progn ,@body)
             (anvil-org-index--execute ,db-sym "COMMIT"))
         (error
          (ignore-errors (anvil-org-index--execute ,db-sym "ROLLBACK"))
          (signal (car err) (cdr err)))))))

;;; Schema

(defconst anvil-org-index--ddl
  '("CREATE TABLE IF NOT EXISTS schema_meta (
       version INTEGER PRIMARY KEY)"

    "CREATE TABLE IF NOT EXISTS file (
       id          INTEGER PRIMARY KEY,
       path        TEXT UNIQUE NOT NULL,
       mtime       INTEGER NOT NULL,
       size        INTEGER NOT NULL,
       indexed_at  INTEGER NOT NULL,
       schema_ver  INTEGER NOT NULL DEFAULT 1)"

    "CREATE TABLE IF NOT EXISTS headline (
       id          INTEGER PRIMARY KEY,
       file_id     INTEGER NOT NULL REFERENCES file(id) ON DELETE CASCADE,
       parent_id   INTEGER,
       level       INTEGER NOT NULL,
       title       TEXT NOT NULL,
       todo        TEXT,
       priority    TEXT,
       scheduled   TEXT,
       deadline    TEXT,
       closed      TEXT,
       org_id      TEXT,
       position    INTEGER NOT NULL,
       line_start  INTEGER NOT NULL,
       line_end    INTEGER)"

    "CREATE INDEX IF NOT EXISTS idx_headline_file ON headline(file_id)"
    "CREATE INDEX IF NOT EXISTS idx_headline_todo ON headline(todo)"
    "CREATE INDEX IF NOT EXISTS idx_headline_id ON headline(org_id) WHERE org_id IS NOT NULL"
    "CREATE INDEX IF NOT EXISTS idx_headline_sched ON headline(scheduled) WHERE scheduled IS NOT NULL"

    "CREATE TABLE IF NOT EXISTS tag (
       headline_id INTEGER NOT NULL REFERENCES headline(id) ON DELETE CASCADE,
       tag         TEXT NOT NULL,
       PRIMARY KEY (headline_id, tag))"

    "CREATE INDEX IF NOT EXISTS idx_tag_tag ON tag(tag)"

    "CREATE TABLE IF NOT EXISTS property (
       headline_id INTEGER NOT NULL REFERENCES headline(id) ON DELETE CASCADE,
       key         TEXT NOT NULL,
       value       TEXT NOT NULL,
       PRIMARY KEY (headline_id, key))"

    "CREATE INDEX IF NOT EXISTS idx_property_key ON property(key)")
  "List of DDL statements applied on DB open.")

(defun anvil-org-index--apply-ddl (db)
  "Apply DDL + pragmas + schema version record to DB."
  (anvil-org-index--execute db "PRAGMA journal_mode = WAL")
  (anvil-org-index--execute db "PRAGMA foreign_keys = ON")
  (dolist (stmt anvil-org-index--ddl)
    (anvil-org-index--execute db stmt))
  (anvil-org-index--execute
   db "INSERT OR IGNORE INTO schema_meta(version) VALUES (?)"
   (list anvil-org-index-schema-version)))

;;; File discovery

(defun anvil-org-index--excluded-p (path)
  "Return non-nil if PATH matches any `anvil-org-index-exclude-patterns'."
  (cl-some (lambda (re) (string-match-p re path))
           anvil-org-index-exclude-patterns))

(defun anvil-org-index--collect-files (&optional paths)
  "Return absolute paths of all *.org files under PATHS.
PATHS defaults to `anvil-org-index-paths'."
  (let ((roots (or paths anvil-org-index-paths))
        (acc nil))
    (dolist (root roots)
      (let ((abs (expand-file-name root)))
        (if (not (file-directory-p abs))
            (message "anvil-org-index: skipping missing path %s" abs)
          (dolist (f (directory-files-recursively abs "\\.org\\'" nil))
            (unless (anvil-org-index--excluded-p f)
              (push (expand-file-name f) acc))))))
    (nreverse acc)))

;;; Scanner — pure regexp, does not load org-mode

(defun anvil-org-index--parse-headline-title (raw level)
  "Parse RAW headline text (without leading stars) at LEVEL into a plist.
Extracts TODO keyword, priority cookie [#A], trailing tags :t1:t2:,
and the clean title."
  (let ((s raw) todo prio tags)
    (when (string-match "\\`\\([A-Z]+\\(?:-[A-Z]+\\)?\\)[ \t]+\\(.*\\)\\'" s)
      (let ((kw (match-string 1 s)))
        (when (member kw anvil-org-index-todo-keywords)
          (setq todo kw
                s (match-string 2 s)))))
    (when (string-match "\\`\\[#\\([A-Z]\\)\\][ \t]+\\(.*\\)\\'" s)
      (setq prio (match-string 1 s)
            s (match-string 2 s)))
    (when (string-match "[ \t]+\\(:[A-Za-z0-9_@#%:]+:\\)[ \t]*\\'" s)
      ;; Capture match data before calling `split-string' which resets it.
      (let ((beg    (match-beginning 0))
            (tagstr (match-string 1 s)))
        (setq tags (split-string tagstr ":" t))
        (setq s    (substring s 0 beg))))
    (list :level level
          :todo todo
          :priority prio
          :tags tags
          :title (string-trim s))))

(defun anvil-org-index--scan-buffer ()
  "Scan the current buffer as org text.
Return a list of headline plists in document order.  The buffer
is expected to contain org syntax as plain text; org-mode is NOT
loaded."
  (save-excursion
    (goto-char (point-min))
    (let ((results nil)
          (current nil)
          (in-properties nil)
          (line 1))
      (while (not (eobp))
        (let* ((bol (line-beginning-position))
               (eol (line-end-position))
               (text (buffer-substring-no-properties bol eol)))
          (cond
           ;; headline
           ((string-match "\\`\\(\\*+\\)[ \t]+\\(.*\\)\\'" text)
            (when current
              (setq current (plist-put current :line-end (1- line)))
              (push current results))
            (let* ((level (length (match-string 1 text)))
                   (rest  (match-string 2 text))
                   (parsed (anvil-org-index--parse-headline-title rest level)))
              (setq current (append parsed
                                    (list :position bol
                                          :line-start line
                                          :scheduled nil
                                          :deadline nil
                                          :closed nil
                                          :org-id nil
                                          :properties nil))
                    in-properties nil)))
           ;; :PROPERTIES: start
           ((and current (string-match-p "\\`[ \t]*:PROPERTIES:[ \t]*\\'" text))
            (setq in-properties t))
           ;; :END:
           ((and current in-properties
                 (string-match-p "\\`[ \t]*:END:[ \t]*\\'" text))
            (setq in-properties nil))
           ;; property line
           ((and current in-properties
                 (string-match "\\`[ \t]*:\\([^:\n\t ]+\\):[ \t]+\\(.*\\)\\'" text))
            (let ((key (match-string 1 text))
                  (val (string-trim (match-string 2 text))))
              (setq current
                    (plist-put current :properties
                               (cons (cons key val)
                                     (plist-get current :properties))))
              (when (string-equal (upcase key) "ID")
                (setq current (plist-put current :org-id val)))))
           ;; SCHEDULED / DEADLINE / CLOSED on planning line.
           ;; A single planning line may carry multiple keywords, so
           ;; scan all matches (not just the first).
           ((and current
                 (string-match-p
                  "\\b\\(?:SCHEDULED\\|DEADLINE\\|CLOSED\\):"
                  text))
            (let ((start 0))
              (while (string-match
                      "\\b\\(SCHEDULED\\|DEADLINE\\|CLOSED\\):[ \t]*\\(<[^>\n]+>\\|\\[[^]\n]+\\]\\)"
                      text start)
                (let ((which (upcase (match-string 1 text)))
                      (ts    (match-string 2 text))
                      (end   (match-end 0)))
                  (setq current
                        (plist-put current
                                   (pcase which
                                     ("SCHEDULED" :scheduled)
                                     ("DEADLINE"  :deadline)
                                     ("CLOSED"    :closed))
                                   ts))
                  (setq start end))))))
          (goto-char (1+ eol))
          (cl-incf line)))
      (when current
        (setq current (plist-put current :line-end (1- line)))
        (push current results))
      (nreverse results))))

(defun anvil-org-index--scan-file (path)
  "Return list of headline plists for PATH."
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8-unix))
      (insert-file-contents path))
    (anvil-org-index--scan-buffer)))

;;; Insertion helpers

(defun anvil-org-index--insert-file-row (db path)
  "Delete existing FILE row for PATH if any, then insert fresh.
Return the new file id."
  (let* ((attrs (file-attributes path))
         (mtime (when attrs (floor (float-time (file-attribute-modification-time attrs)))))
         (size  (when attrs (file-attribute-size attrs)))
         (now   (floor (float-time))))
    (anvil-org-index--execute db "DELETE FROM file WHERE path = ?" (list path))
    (anvil-org-index--execute
     db
     "INSERT INTO file (path, mtime, size, indexed_at, schema_ver) VALUES (?, ?, ?, ?, ?)"
     (list path mtime size now anvil-org-index-schema-version))
    (caar (anvil-org-index--select db "SELECT last_insert_rowid()"))))

(defun anvil-org-index--insert-headline (db file-id parent-id hl)
  "Insert one headline HL under FILE-ID with optional PARENT-ID.
Return the new headline id."
  (anvil-org-index--execute
   db
   "INSERT INTO headline
      (file_id, parent_id, level, title, todo, priority,
       scheduled, deadline, closed, org_id, position, line_start, line_end)
    VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?)"
   (list file-id
         parent-id
         (plist-get hl :level)
         (plist-get hl :title)
         (plist-get hl :todo)
         (plist-get hl :priority)
         (plist-get hl :scheduled)
         (plist-get hl :deadline)
         (plist-get hl :closed)
         (plist-get hl :org-id)
         (plist-get hl :position)
         (plist-get hl :line-start)
         (plist-get hl :line-end)))
  (let ((hid (caar (anvil-org-index--select db "SELECT last_insert_rowid()"))))
    (dolist (tag (plist-get hl :tags))
      (anvil-org-index--execute
       db "INSERT OR IGNORE INTO tag (headline_id, tag) VALUES (?, ?)"
       (list hid tag)))
    (dolist (kv (plist-get hl :properties))
      (anvil-org-index--execute
       db "INSERT OR REPLACE INTO property (headline_id, key, value) VALUES (?, ?, ?)"
       (list hid (car kv) (cdr kv))))
    hid))

(defun anvil-org-index--ingest-file (db path)
  "Re-index a single PATH into DB.  Return list (headline-count)."
  (let* ((headlines (anvil-org-index--scan-file path))
         (file-id   (anvil-org-index--insert-file-row db path))
         ;; stack: (level . id) pairs, deepest on top
         (stack nil))
    (dolist (hl headlines)
      (let ((level (plist-get hl :level)))
        (while (and stack (>= (caar stack) level))
          (pop stack))
        (let* ((parent-id (and stack (cdar stack)))
               (hid (anvil-org-index--insert-headline db file-id parent-id hl)))
          (push (cons level hid) stack))))
    (list :headline-count (length headlines))))

;;; Public API — Phase 1

;;;###autoload
(defun anvil-org-index-enable ()
  "Initialize backend, open DB, apply DDL.  Idempotent."
  (interactive)
  (unless anvil-org-index--backend
    (setq anvil-org-index--backend (anvil-org-index--detect-backend)))
  (unless (and anvil-org-index--db
               (or (not (eq anvil-org-index--backend 'builtin))
                   (sqlitep anvil-org-index--db)))
    (let ((dir (file-name-directory anvil-org-index-db-path)))
      (unless (file-directory-p dir) (make-directory dir t)))
    (setq anvil-org-index--db
          (anvil-org-index--open anvil-org-index-db-path))
    (anvil-org-index--apply-ddl anvil-org-index--db))
  (message "anvil-org-index: enabled (backend=%s db=%s)"
           anvil-org-index--backend
           anvil-org-index-db-path))

;;;###autoload
(defun anvil-org-index-disable ()
  "Close DB.  Safe to call multiple times."
  (interactive)
  (when anvil-org-index--db
    (anvil-org-index--close anvil-org-index--db)
    (setq anvil-org-index--db nil))
  (message "anvil-org-index: disabled"))

;;;###autoload
(defun anvil-org-index-rebuild (&optional paths)
  "Rebuild the index over PATHS (defaults to `anvil-org-index-paths').
Existing rows for the touched files are replaced.  Runs inside a
single transaction.  Returns a summary plist."
  (interactive)
  (unless anvil-org-index--db (anvil-org-index-enable))
  (let* ((files (anvil-org-index--collect-files paths))
         (start (float-time))
         (total 0))
    (anvil-org-index--with-transaction anvil-org-index--db
      (dolist (f files)
        (let ((info (anvil-org-index--ingest-file anvil-org-index--db f)))
          (cl-incf total (plist-get info :headline-count)))))
    (let ((elapsed (- (float-time) start)))
      (message "anvil-org-index: rebuild %d file(s), %d headline(s) in %.2fs"
               (length files) total elapsed)
      (list :files (length files)
            :headlines total
            :elapsed-sec elapsed))))

;;;###autoload
(defun anvil-org-index-status ()
  "Show a summary of the current index state."
  (interactive)
  (if (not anvil-org-index--db)
      (message "anvil-org-index: not enabled")
    (let* ((files (caar (anvil-org-index--select
                         anvil-org-index--db "SELECT COUNT(*) FROM file")))
           (heads (caar (anvil-org-index--select
                         anvil-org-index--db "SELECT COUNT(*) FROM headline")))
           (tags  (caar (anvil-org-index--select
                         anvil-org-index--db "SELECT COUNT(DISTINCT tag) FROM tag")))
           (size  (if (file-exists-p anvil-org-index-db-path)
                      (file-attribute-size
                       (file-attributes anvil-org-index-db-path))
                    0)))
      (message "anvil-org-index: %d file(s), %d headline(s), %d tag(s), db=%s (%.1fKB)"
               files heads tags
               anvil-org-index-db-path
               (/ size 1024.0)))))

(provide 'anvil-org-index)
;;; anvil-org-index.el ends here
