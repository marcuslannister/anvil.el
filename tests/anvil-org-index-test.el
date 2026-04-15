;;; anvil-org-index-test.el --- Tests for anvil-org-index -*- lexical-binding: t; -*-

;;; Commentary:

;; Scanner unit tests and a rebuild smoke test that writes a temporary
;; org file, ingests it, and verifies the resulting rows.  The rebuild
;; test is skipped gracefully on Emacs < 29 without built-in sqlite.

;;; Code:

(require 'ert)
(require 'anvil-org-index)

;;;; Scanner unit tests

(defun anvil-org-index-test--scan (content)
  "Return the headline plist list produced by scanning CONTENT."
  (with-temp-buffer
    (insert content)
    (anvil-org-index--scan-buffer)))

(ert-deftest anvil-org-index-test-scan-basic-headline ()
  "A single top-level headline is captured with level and title."
  (let* ((res (anvil-org-index-test--scan "* Hello world\n"))
         (hl  (car res)))
    (should (= 1 (length res)))
    (should (= 1 (plist-get hl :level)))
    (should (equal "Hello world" (plist-get hl :title)))
    (should (null (plist-get hl :todo)))
    (should (null (plist-get hl :tags)))))

(ert-deftest anvil-org-index-test-scan-todo-priority-tags ()
  "TODO keyword, priority cookie and trailing tags are stripped from title."
  (let* ((res (anvil-org-index-test--scan
               "** TODO [#A] Ship the feature   :work:urgent:\n"))
         (hl  (car res)))
    (should (= 2 (plist-get hl :level)))
    (should (equal "TODO" (plist-get hl :todo)))
    (should (equal "A"    (plist-get hl :priority)))
    (should (equal "Ship the feature" (plist-get hl :title)))
    (should (equal '("work" "urgent") (plist-get hl :tags)))))

(ert-deftest anvil-org-index-test-scan-done-keyword ()
  "DONE is recognized as a TODO state."
  (let* ((res (anvil-org-index-test--scan "* DONE Finished task\n"))
         (hl  (car res)))
    (should (equal "DONE" (plist-get hl :todo)))
    (should (equal "Finished task" (plist-get hl :title)))))

(ert-deftest anvil-org-index-test-scan-non-todo-uppercase-not-stripped ()
  "Arbitrary leading uppercase word that is not a TODO keyword stays in title."
  (let* ((res (anvil-org-index-test--scan "* FOO matters\n"))
         (hl  (car res)))
    (should (null (plist-get hl :todo)))
    (should (equal "FOO matters" (plist-get hl :title)))))

(ert-deftest anvil-org-index-test-scan-properties-and-id ()
  "PROPERTIES drawer entries are collected; :ID: also lifted to :org-id."
  (let* ((content "* Item
:PROPERTIES:
:ID:       abc-123
:CATEGORY: work
:END:
")
         (res (anvil-org-index-test--scan content))
         (hl  (car res))
         (props (plist-get hl :properties)))
    (should (equal "abc-123" (plist-get hl :org-id)))
    (should (equal "abc-123" (cdr (assoc "ID" props))))
    (should (equal "work"    (cdr (assoc "CATEGORY" props))))))

(ert-deftest anvil-org-index-test-scan-scheduled-deadline ()
  "SCHEDULED and DEADLINE timestamps attach to the current headline."
  (let* ((content "* Task
SCHEDULED: <2026-04-15 Wed> DEADLINE: <2026-04-20 Mon>
"))
    (let ((hl (car (anvil-org-index-test--scan content))))
      (should (equal "<2026-04-15 Wed>" (plist-get hl :scheduled)))
      (should (equal "<2026-04-20 Mon>" (plist-get hl :deadline))))))

(ert-deftest anvil-org-index-test-scan-multiple-headlines-nested ()
  "Level order and count are preserved across nested siblings."
  (let* ((content "* Parent
** Child 1
** Child 2
*** Grandchild
* Sibling
")
         (res (anvil-org-index-test--scan content))
         (levels (mapcar (lambda (p) (plist-get p :level)) res)))
    (should (= 5 (length res)))
    (should (equal '(1 2 2 3 1) levels))))

(ert-deftest anvil-org-index-test-scan-line-ranges ()
  "line-start and line-end bracket the headline body correctly."
  (let* ((content "* One\nbody\n* Two\nmore\n"))
    (let ((res (anvil-org-index-test--scan content)))
      (should (= 1 (plist-get (nth 0 res) :line-start)))
      (should (= 2 (plist-get (nth 0 res) :line-end)))
      (should (= 3 (plist-get (nth 1 res) :line-start))))))

;;;; Parent-child linking via stack

(ert-deftest anvil-org-index-test-parent-link-from-ingest ()
  "Stack-based parent resolution matches expected parent indices.
This exercises the same traversal logic used in
`anvil-org-index--ingest-file' without requiring a DB."
  (let* ((content "* A
** A1
*** A1a
** A2
* B
")
         (headlines (anvil-org-index-test--scan content))
         (parents nil)
         (stack nil)
         (i 0))
    (dolist (hl headlines)
      (let ((level (plist-get hl :level)))
        (while (and stack (>= (caar stack) level))
          (pop stack))
        (push (and stack (cdar stack)) parents)
        (push (cons level i) stack)
        (cl-incf i)))
    (setq parents (nreverse parents))
    ;; indices:   0=A  1=A1  2=A1a  3=A2  4=B
    ;; parents:  nil    0     1      0   nil
    (should (equal '(nil 0 1 0 nil) parents))))

;;;; DB smoke test — skipped if no sqlite backend

(defun anvil-org-index-test--have-sqlite ()
  (and (fboundp 'sqlite-available-p) (sqlite-available-p)))

(ert-deftest anvil-org-index-test-rebuild-roundtrip ()
  "Write temp org, rebuild index, verify counts via status queries."
  (skip-unless (anvil-org-index-test--have-sqlite))
  (let* ((tmpdir (make-temp-file "anvil-org-index-test-" t))
         (orgfile (expand-file-name "sample.org" tmpdir))
         (dbfile (expand-file-name "test-index.db" tmpdir))
         (anvil-org-index-db-path dbfile)
         (anvil-org-index-paths (list tmpdir))
         (anvil-org-index--backend nil)
         (anvil-org-index--db nil))
    (unwind-protect
        (progn
          (let ((coding-system-for-write 'utf-8-unix))
            (write-region
             "* TODO [#B] One :alpha:
:PROPERTIES:
:ID: root-1
:END:
SCHEDULED: <2026-04-15 Wed>
** DONE Two :alpha:beta:
* Sibling
"
             nil orgfile nil 'silent))
          (anvil-org-index-enable)
          (let ((summary (anvil-org-index-rebuild)))
            (should (= 1 (plist-get summary :files)))
            (should (= 3 (plist-get summary :headlines))))
          (let ((rows (anvil-org-index--select
                       anvil-org-index--db
                       "SELECT title, todo, priority, org_id FROM headline ORDER BY line_start")))
            (should (equal '("One" "TODO" "B" "root-1") (car rows)))
            (should (equal '("Two" "DONE" nil nil)      (cadr rows)))
            (should (equal '("Sibling" nil nil nil)     (caddr rows))))
          (let ((tag-rows (anvil-org-index--select
                           anvil-org-index--db
                           "SELECT DISTINCT tag FROM tag ORDER BY tag")))
            (should (equal '(("alpha") ("beta")) tag-rows))))
      (ignore-errors (anvil-org-index-disable))
      (ignore-errors (delete-directory tmpdir t)))))

;;;; Phase 2 — incremental refresh

(defun anvil-org-index-test--write (path content)
  "Write CONTENT to PATH as UTF-8."
  (let ((coding-system-for-write 'utf-8-unix))
    (write-region content nil path nil 'silent)))

(defun anvil-org-index-test--touch-future (path seconds)
  "Set PATH mtime to SECONDS in the future of its current mtime.
Uses a shell `touch' via `set-file-times' for portability."
  (let* ((attrs (file-attributes path))
         (now   (float-time (file-attribute-modification-time attrs)))
         (new   (seconds-to-time (+ now seconds))))
    (set-file-times path new new)))

(ert-deftest anvil-org-index-test-refresh-unchanged-is-noop ()
  "refresh-if-stale on an unchanged file reports outcome=unchanged."
  (skip-unless (anvil-org-index-test--have-sqlite))
  (let* ((tmpdir (make-temp-file "anvil-idx-" t))
         (f (expand-file-name "a.org" tmpdir))
         (db (expand-file-name "i.db" tmpdir))
         (anvil-org-index-db-path db)
         (anvil-org-index-paths (list tmpdir))
         (anvil-org-index--backend nil)
         (anvil-org-index--db nil))
    (unwind-protect
        (progn
          (anvil-org-index-test--write f "* One\n")
          (anvil-org-index-enable)
          (anvil-org-index-rebuild)
          (let ((res (anvil-org-index-refresh-if-stale f)))
            (should (eq 'unchanged (plist-get res :outcome)))))
      (ignore-errors (anvil-org-index-disable))
      (ignore-errors (delete-directory tmpdir t)))))

(ert-deftest anvil-org-index-test-refresh-detects-modification ()
  "Touching a file to a newer mtime triggers reindex."
  (skip-unless (anvil-org-index-test--have-sqlite))
  (let* ((tmpdir (make-temp-file "anvil-idx-" t))
         (f (expand-file-name "a.org" tmpdir))
         (db (expand-file-name "i.db" tmpdir))
         (anvil-org-index-db-path db)
         (anvil-org-index-paths (list tmpdir))
         (anvil-org-index--backend nil)
         (anvil-org-index--db nil))
    (unwind-protect
        (progn
          (anvil-org-index-test--write f "* One\n")
          (anvil-org-index-enable)
          (anvil-org-index-rebuild)
          (should (= 1 (caar (anvil-org-index--select
                              anvil-org-index--db
                              "SELECT COUNT(*) FROM headline"))))
          (anvil-org-index-test--write f "* One\n* Two\n")
          (anvil-org-index-test--touch-future f 5)
          (let ((res (anvil-org-index-refresh-if-stale f)))
            (should (eq 'reindexed (plist-get res :outcome))))
          (should (= 2 (caar (anvil-org-index--select
                              anvil-org-index--db
                              "SELECT COUNT(*) FROM headline")))))
      (ignore-errors (anvil-org-index-disable))
      (ignore-errors (delete-directory tmpdir t)))))

(ert-deftest anvil-org-index-test-refresh-removes-deleted-file ()
  "Deleting a file from disk then refreshing drops its rows."
  (skip-unless (anvil-org-index-test--have-sqlite))
  (let* ((tmpdir (make-temp-file "anvil-idx-" t))
         (f (expand-file-name "a.org" tmpdir))
         (db (expand-file-name "i.db" tmpdir))
         (anvil-org-index-db-path db)
         (anvil-org-index-paths (list tmpdir))
         (anvil-org-index--backend nil)
         (anvil-org-index--db nil))
    (unwind-protect
        (progn
          (anvil-org-index-test--write f "* One\n")
          (anvil-org-index-enable)
          (anvil-org-index-rebuild)
          (delete-file f)
          (let ((res (anvil-org-index-refresh-if-stale)))
            (should (= 0 (plist-get res :scanned)))
            (should (= 1 (plist-get res :removed))))
          (should (= 0 (caar (anvil-org-index--select
                              anvil-org-index--db
                              "SELECT COUNT(*) FROM file")))))
      (ignore-errors (anvil-org-index-disable))
      (ignore-errors (delete-directory tmpdir t)))))

(ert-deftest anvil-org-index-test-refresh-all-mixed-changes ()
  "refresh-all handles added / reindexed / removed / unchanged together."
  (skip-unless (anvil-org-index-test--have-sqlite))
  (let* ((tmpdir (make-temp-file "anvil-idx-" t))
         (a (expand-file-name "a.org" tmpdir))  ; unchanged
         (b (expand-file-name "b.org" tmpdir))  ; will be modified
         (c (expand-file-name "c.org" tmpdir))  ; will be deleted
         (d (expand-file-name "d.org" tmpdir))  ; will be added
         (db (expand-file-name "i.db" tmpdir))
         (anvil-org-index-db-path db)
         (anvil-org-index-paths (list tmpdir))
         (anvil-org-index--backend nil)
         (anvil-org-index--db nil))
    (unwind-protect
        (progn
          (anvil-org-index-test--write a "* A\n")
          (anvil-org-index-test--write b "* B\n")
          (anvil-org-index-test--write c "* C\n")
          (anvil-org-index-enable)
          (anvil-org-index-rebuild)
          ;; mutate
          (anvil-org-index-test--write b "* B\n* B2\n")
          (anvil-org-index-test--touch-future b 5)
          (delete-file c)
          (anvil-org-index-test--write d "* D\n")
          (let ((res (anvil-org-index-refresh-if-stale)))
            (should (= 3 (plist-get res :scanned)))  ; a b d on disk
            (should (= 1 (plist-get res :added)))
            (should (= 1 (plist-get res :reindexed)))
            (should (= 1 (plist-get res :removed)))
            (should (= 1 (plist-get res :unchanged)))))
      (ignore-errors (anvil-org-index-disable))
      (ignore-errors (delete-directory tmpdir t)))))

;;; anvil-org-index-test.el ends here
