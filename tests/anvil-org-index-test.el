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

;;; anvil-org-index-test.el ends here
