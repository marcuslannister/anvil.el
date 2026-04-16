;;; anvil-dev.el --- Developer / ops helpers for anvil -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Fujisawa Electric Management Office

;; This file is part of anvil.el.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Small tools that are *about* anvil rather than part of its
;; operation.  Currently:
;;
;;   - `anvil-self-sync-check'
;;     — inspect the git state of the anvil clone that Emacs loaded,
;;       and optionally compare it to a separate dev checkout so the
;;       "installed tree silently diverged from the dev tree" bug
;;       (2026-04-16) is caught in one MCP call.
;;   - `anvil-journal-append'
;;     — idempotently append a `*** MEMO AI: ...' entry under the
;;       day's `** NOTE 作業ログ' section of an org journal, creating
;;       the section if missing.  Encodes the CLAUDE.md convention
;;       for session-log entries so future sessions do not have to
;;       re-implement the find-or-create logic.
;;
;; Enable via `(add-to-list 'anvil-optional-modules 'dev)' in init.

;;; Code:

(require 'cl-lib)
(require 'anvil-server)

(defgroup anvil-dev nil
  "Developer / ops helpers for anvil."
  :group 'anvil
  :prefix "anvil-dev-")

(defcustom anvil-dev-source-path nil
  "Optional path to a separate dev checkout of anvil.el.
When non-nil, `anvil-self-sync-check' fetches its HEAD and
compares it against the installed clone's HEAD so `git pull'
gaps surface before the next daemon restart."
  :type '(choice (const :tag "No dev clone" nil) directory)
  :group 'anvil-dev)

(defconst anvil-dev--server-id "emacs-eval"
  "Server ID for the dev-* MCP tools.")

;;;; --- internal ------------------------------------------------------------

(defun anvil-dev--git-at (dir &rest args)
  "Run `git ARGS' inside DIR and return trimmed stdout, or nil on failure."
  (when (and dir (file-directory-p dir))
    (with-temp-buffer
      (let* ((default-directory (file-name-as-directory dir))
             (status (apply #'call-process "git" nil t nil args)))
        (when (and (integerp status) (zerop status))
          (string-trim (buffer-string)))))))

(defun anvil-dev--short-sha (sha)
  "Return the first 7 chars of SHA, or SHA itself if shorter / nil."
  (and sha (stringp sha)
       (if (> (length sha) 7) (substring sha 0 7) sha)))

(defun anvil-dev--git-state (dir)
  "Return a plist describing the git state of DIR (nil if not a worktree).
Keys: :head :branch :dirty-count."
  (let ((head (anvil-dev--git-at dir "rev-parse" "HEAD")))
    (when head
      (let* ((branch (anvil-dev--git-at dir "rev-parse" "--abbrev-ref" "HEAD"))
             (porc (anvil-dev--git-at dir "status" "--porcelain"))
             (dirty (if (and porc (not (string-empty-p porc)))
                        (length (split-string porc "\n" t))
                      0)))
        (list :head head :branch branch :dirty-count dirty)))))

(defun anvil-dev--derive-warning (src-dir installed-state dev-dir dev-state)
  "Produce a human-readable warning string, or nil when all is well."
  (cond
   ((null src-dir)
    "anvil-server not located — library not loaded")
   ((null installed-state)
    (format "installed dir %s is not a git worktree" src-dir))
   ((and dev-dir (null dev-state))
    (format "dev-source-path %s is not a git worktree" dev-dir))
   ((and dev-state
         (not (equal (plist-get installed-state :head)
                     (plist-get dev-state :head))))
    (format "installed HEAD %s ≠ dev HEAD %s — run `git pull' in %s"
            (anvil-dev--short-sha (plist-get installed-state :head))
            (anvil-dev--short-sha (plist-get dev-state :head))
            src-dir))))

;;;; --- public --------------------------------------------------------------

;;;###autoload
(defun anvil-self-sync-check ()
  "Report anvil's installed git state, and mismatch vs the dev checkout.

Returns a plist:
  :installed-dir         where `anvil-server' was loaded from
  :installed-head        HEAD SHA of that worktree (nil if not a git repo)
  :installed-branch      current branch of the installed clone
  :installed-dirty-count number of modified / untracked files
  :dev-dir               `anvil-dev-source-path' (or nil)
  :dev-head              HEAD SHA of the dev clone (nil / not set)
  :dev-branch            branch of the dev clone
  :in-sync               t when HEADs match OR when no dev-dir is configured
  :warning               short human string when something is off (nil = OK)

Motivation: 2026-04-16 reproduced the \"old anvil-worker loaded\"
trap where the running daemon read an outdated default because a
second clone (`external-packages/anvil.el/`) stayed behind the
dev tree.  One call to this helper now surfaces that mismatch."
  (let* ((src-file (locate-library "anvil-server"))
         (src-dir  (and src-file (file-name-directory src-file)))
         (installed (and src-dir (anvil-dev--git-state src-dir)))
         (dev-dir  anvil-dev-source-path)
         (dev-st   (and dev-dir (anvil-dev--git-state dev-dir)))
         (in-sync  (or (null dev-dir)
                       (and installed dev-st
                            (equal (plist-get installed :head)
                                   (plist-get dev-st :head)))))
         (warning  (anvil-dev--derive-warning
                    src-dir installed dev-dir dev-st)))
    (list :installed-dir         src-dir
          :installed-head        (plist-get installed :head)
          :installed-branch      (plist-get installed :branch)
          :installed-dirty-count (or (plist-get installed :dirty-count) 0)
          :dev-dir               dev-dir
          :dev-head              (plist-get dev-st :head)
          :dev-branch            (plist-get dev-st :branch)
          :in-sync               (and in-sync t)
          :warning               warning)))

(defun anvil-dev--tool-self-sync-check ()
  "MCP wrapper for `anvil-self-sync-check'.

MCP Parameters: none.  Returns a printed plist comparing the
installed anvil clone's git HEAD with `anvil-dev-source-path'."
  (anvil-server-with-error-handling
   (format "%S" (anvil-self-sync-check))))

;;;; --- anvil-journal-append -----------------------------------------------

(defcustom anvil-journal-worklog-heading "作業ログ"
  "Heading text used to identify the day's worklog section.
Combined with the date (e.g. \"作業ログ <2026-04-16 Thu>\") and a
NOTE TODO-state prefix to form the full level-2 heading."
  :type 'string
  :group 'anvil-dev)

(defconst anvil-dev--weekday-abbrevs
  ["Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat"]
  "English 3-letter weekday abbreviations, indexed by `decode-time' dow.")

(defconst anvil-dev--weekday-full-names
  ["Sunday" "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday"]
  "English full weekday names.  Matches org's timestamp + day-heading style.")

(defun anvil-dev--weekday-abbrev (date)
  "Return English 3-letter weekday abbreviation for ISO DATE (\"YYYY-MM-DD\").
Independent of `system-time-locale' — Windows daemons with CP932
locale return 1-byte Japanese weekday chars from `format-time-string',
which is why we ship a fixed lookup table."
  (let* ((time (date-to-time (concat date " 00:00:00")))
         (dow  (nth 6 (decode-time time))))
    (aref anvil-dev--weekday-abbrevs dow)))

(defun anvil-dev--date-day-heading (date)
  "Return the level-1 day heading text for DATE (\"YYYY-MM-DD\").
Example: \"2026-04-16-Thursday\"."
  (let* ((time (date-to-time (concat date " 00:00:00")))
         (dow  (nth 6 (decode-time time))))
    (concat date "-" (aref anvil-dev--weekday-full-names dow))))

(defun anvil-dev--org-day-bounds (date)
  "Return cons (START . END) of the level-1 subtree for DATE.
Assumes the current buffer holds an org journal where each day
is a level-1 heading like `* 2026-04-16-Thursday'.  Returns nil
when no such heading exists."
  (save-excursion
    (goto-char (point-min))
    (let ((day-heading
           (concat "^\\* " (regexp-quote (anvil-dev--date-day-heading date))
                   "\\(?:[ \t]\\|$\\)")))
      (when (re-search-forward day-heading nil t)
        (let ((start (match-beginning 0))
              (end (or (save-excursion
                         (forward-line 1)
                         (and (re-search-forward "^\\* " nil t)
                              (match-beginning 0)))
                       (point-max))))
          (cons start end))))))

(defun anvil-dev--ensure-worklog-section (date)
  "Find (or create) the `** NOTE 作業ログ <DATE ...>' section for DATE.
Must be called with the org journal buffer current and narrowed
nothing.  Returns cons (SECTION-START . SECTION-END).  When the
section is freshly inserted SECTION-END is the same as SECTION-START."
  (let* ((bounds (or (anvil-dev--org-day-bounds date)
                     (error "anvil-dev: no `* %s' day heading in journal"
                            (anvil-dev--date-day-heading date))))
         (day-end (cdr bounds))
         (section-re
          (concat "^\\*\\* NOTE "
                  (regexp-quote anvil-journal-worklog-heading)
                  " <" (regexp-quote date))))
    (save-excursion
      (goto-char (car bounds))
      (if (re-search-forward section-re day-end t)
          (let ((sect-start (match-beginning 0))
                (sect-end (or (save-excursion
                                (forward-line 1)
                                (and (re-search-forward
                                      "^\\*\\{1,2\\} " day-end t)
                                     (match-beginning 0)))
                              day-end)))
            (cons sect-start sect-end))
        ;; Not found — insert at day-end.
        (goto-char day-end)
        (let ((start (point))
              (wday (anvil-dev--weekday-abbrev date)))
          (insert (format "** NOTE %s <%s %s>\n"
                          anvil-journal-worklog-heading date wday))
          (cons start (point)))))))

;;;###autoload
(defun anvil-journal-append (file date title body)
  "Append a `*** MEMO AI: TITLE <DATE WDAY>' entry to FILE's worklog.

FILE is the path to an org journal.  DATE is \"YYYY-MM-DD\".  TITLE
is the memo's short name.  BODY is the org-mode text inserted
under the MEMO heading (a blank trailing newline is added if
missing).

Creates the day's `** NOTE 作業ログ <DATE WDAY>' section when
absent, then inserts the MEMO at the section's end.  Saves the
buffer afterwards.  Returns a plist describing what happened:
  (:file FILE :date DATE :title TITLE :section-created BOOL
   :bytes-inserted N)

Does *not* touch any other existing NOTE / MEMO / DONE / CANCEL
entries — only appends.  Fails with `user-error' when the day's
level-1 heading (e.g. `* 2026-04-16-Thursday') is missing."
  (let* ((coding-system-for-read 'utf-8)
         (buf (find-file-noselect file))
         (wday (anvil-dev--weekday-abbrev date))
         (memo-header (format "*** MEMO AI: %s <%s %s>\n" title date wday))
         (body (if (and (stringp body) (> (length body) 0)
                        (not (string-suffix-p "\n" body)))
                   (concat body "\n")
                 body))
         (memo-text (concat memo-header
                            (or body "")
                            "\n"))
         section-created
         inserted)
    (with-current-buffer buf
      ;; Org journals are UTF-8 by convention; pin the coding here so
      ;; Windows CP932 daemons don't silently re-save as Shift-JIS.
      (set-buffer-file-coding-system 'utf-8-unix)
      (save-excursion
        (let* ((pre-exists
                (save-excursion
                  (goto-char (point-min))
                  (and (re-search-forward
                        (concat "^\\*\\* NOTE "
                                (regexp-quote anvil-journal-worklog-heading)
                                " <" (regexp-quote date))
                        nil t)
                       t)))
               (bounds (anvil-dev--ensure-worklog-section date))
               (section-end (cdr bounds)))
          (setq section-created (not pre-exists))
          (goto-char section-end)
          (let ((ins-start (point)))
            (insert memo-text)
            (setq inserted (- (point) ins-start)))))
      (let ((coding-system-for-write 'utf-8-unix))
        (save-buffer)))
    (list :file file :date date :title title
          :section-created (and section-created t)
          :bytes-inserted inserted)))

(defun anvil-dev--tool-journal-append (file date title body)
  "MCP wrapper for `anvil-journal-append'.

MCP Parameters:
  file  - Path to the org journal (string, required).
  date  - ISO date \"YYYY-MM-DD\" for the day to append under.
  title - Short title used in the `*** MEMO AI: TITLE' heading.
  body  - Org-mode body text placed under the MEMO heading.
          Include blank lines as needed; a trailing newline is
          added automatically."
  (anvil-server-with-error-handling
   (format "%S" (anvil-journal-append file date title body))))

;;;; --- module lifecycle ----------------------------------------------------

;;;###autoload
(defun anvil-dev-enable ()
  "Register the dev-* MCP tools."
  (anvil-server-register-tool
   #'anvil-dev--tool-self-sync-check
   :id "anvil-self-sync-check"
   :server-id anvil-dev--server-id
   :description
   "Report the installed anvil clone's git HEAD + branch + dirty
count, and (when `anvil-dev-source-path' is set) compare against
the dev checkout to flag unpushed / unpulled divergence before it
causes a silent \"old code loaded\" bug after a daemon restart."
   :read-only t)
  (anvil-server-register-tool
   #'anvil-dev--tool-journal-append
   :id "anvil-journal-append"
   :server-id anvil-dev--server-id
   :description
   "Append a `*** MEMO AI: TITLE <DATE WDAY>' entry to an org
journal.  Finds (or creates) the day's `** NOTE 作業ログ' section
under `* YYYY-MM-DD-Weekday' and inserts the MEMO at the end.
Never touches existing entries — append only.  Returns a plist
(:section-created BOOL :bytes-inserted N).  Encodes the CLAUDE.md
work-log convention so callers skip the ~30 lines of find-or-
create elisp."))

(defun anvil-dev-disable ()
  "Unregister the dev-* MCP tools."
  (dolist (id '("anvil-self-sync-check" "anvil-journal-append"))
    (anvil-server-unregister-tool id anvil-dev--server-id)))

(provide 'anvil-dev)
;;; anvil-dev.el ends here
