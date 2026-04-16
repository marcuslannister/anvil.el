;;; anvil-dev-test.el --- Tests for anvil-dev -*- lexical-binding: t; -*-

;;; Commentary:

;; Exercises `anvil-self-sync-check' and its helpers.  The `git'
;; subprocess is stubbed via `cl-letf' so the tests do not depend
;; on the filesystem containing a real anvil worktree.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'anvil-dev)

(defun anvil-dev-test--call-process-stub-output (buf text)
  "Insert TEXT into BUF, respecting the call-process conventions for t / nil."
  (let ((dest (cond ((eq buf t) (current-buffer))
                    ((bufferp buf) buf)
                    ((stringp buf) (get-buffer buf))
                    (t nil))))
    (when dest
      (with-current-buffer dest (insert text)))))

(defmacro anvil-dev-test--with-git (replies &rest body)
  "Run BODY with `call-process' (of git) returning preset REPLIES.
REPLIES is a list of (ARGS-MATCHER . OUTPUT) pairs.  First hit wins.
Non-git `call-process' calls still signal exit-status 1."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'call-process)
              (lambda (prog &optional _infile buf _disp &rest args)
                (if (equal prog "git")
                    (let ((match (cl-some (lambda (pair)
                                            (and (funcall (car pair) args)
                                                 pair))
                                          ,replies)))
                      (anvil-dev-test--call-process-stub-output
                       buf (or (cdr match) ""))
                      (if match 0 1))
                  1))))
     ,@body))

(defun anvil-dev-test--args-has (substr)
  "Predicate: true when the CALL-PROCESS args contain SUBSTR."
  (lambda (args) (and (member substr args) t)))

(defun anvil-dev-test--make-dir ()
  (let ((d (make-temp-file "anvil-dev-" t)))
    d))

;;;; --- --git-at -----------------------------------------------------------

(ert-deftest anvil-dev-test-git-at-returns-trimmed-stdout ()
  (let ((d (anvil-dev-test--make-dir)))
    (unwind-protect
        (anvil-dev-test--with-git
            (list (cons (anvil-dev-test--args-has "HEAD")
                        "abcdef1234\n"))
          (should (equal "abcdef1234"
                         (anvil-dev--git-at d "rev-parse" "HEAD"))))
      (delete-directory d t))))

(ert-deftest anvil-dev-test-git-at-returns-nil-on-failure ()
  (let ((d (anvil-dev-test--make-dir)))
    (unwind-protect
        (anvil-dev-test--with-git '()
          (should (null (anvil-dev--git-at d "rev-parse" "HEAD"))))
      (delete-directory d t))))

(ert-deftest anvil-dev-test-git-at-rejects-non-directory ()
  (should (null (anvil-dev--git-at "/no/such/dir" "rev-parse" "HEAD")))
  (should (null (anvil-dev--git-at nil "rev-parse" "HEAD"))))

;;;; --- --short-sha --------------------------------------------------------

(ert-deftest anvil-dev-test-short-sha-truncates ()
  (should (equal "abcdef1" (anvil-dev--short-sha "abcdef1234567")))
  (should (equal "abc" (anvil-dev--short-sha "abc")))
  (should (null (anvil-dev--short-sha nil))))

;;;; --- self-sync-check integration ---------------------------------------

(ert-deftest anvil-dev-test-self-sync-check-reports-in-sync ()
  "When installed HEAD equals dev HEAD, :in-sync t and :warning nil."
  (let ((installed (anvil-dev-test--make-dir))
        (dev       (anvil-dev-test--make-dir))
        (anvil-dev-source-path nil))
    (unwind-protect
        (let ((anvil-dev-source-path dev))
          (cl-letf (((symbol-function 'locate-library)
                     (lambda (&rest _)
                       (expand-file-name "anvil-server.el" installed))))
            (anvil-dev-test--with-git
                (list
                 (cons (lambda (args) (member "--porcelain" args)) "")
                 (cons (anvil-dev-test--args-has "--abbrev-ref") "master")
                 (cons (anvil-dev-test--args-has "HEAD")
                       "1111111111111111111111111111111111111111"))
              (let ((res (anvil-self-sync-check)))
                (should (eq t   (plist-get res :in-sync)))
                (should (null   (plist-get res :warning)))
                (should (equal  "1111111111111111111111111111111111111111"
                                (plist-get res :installed-head)))
                (should (equal  "1111111111111111111111111111111111111111"
                                (plist-get res :dev-head)))
                (should (equal  0 (plist-get res :installed-dirty-count)))))))
      (delete-directory installed t)
      (delete-directory dev t))))

(ert-deftest anvil-dev-test-self-sync-check-flags-head-mismatch ()
  "Differing installed / dev HEADs set :in-sync nil and fill :warning."
  (let ((installed (anvil-dev-test--make-dir))
        (dev       (anvil-dev-test--make-dir)))
    (unwind-protect
        (let ((anvil-dev-source-path dev))
          (cl-letf (((symbol-function 'locate-library)
                     (lambda (&rest _)
                       (expand-file-name "anvil-server.el" installed)))
                    ;; Track which dir is asked so we can return
                    ;; different HEADs for installed vs dev.
                    ((symbol-function 'call-process)
                     (lambda (prog &optional _i buf _d &rest args)
                       (when (equal prog "git")
                         (let ((text
                                (cond
                                 ((member "--porcelain" args) "")
                                 ((member "--abbrev-ref" args) "master")
                                 ((equal default-directory
                                         (file-name-as-directory installed))
                                  "aaaaaaa1111111111111111111111111111111111")
                                 (t
                                  "bbbbbbb2222222222222222222222222222222222"))))
                           (anvil-dev-test--call-process-stub-output buf text)
                           0)))))
            (let ((res (anvil-self-sync-check)))
              (should (null (plist-get res :in-sync)))
              (should (stringp (plist-get res :warning)))
              (should (string-match-p "HEAD" (plist-get res :warning))))))
      (delete-directory installed t)
      (delete-directory dev t))))

(ert-deftest anvil-dev-test-self-sync-check-handles-missing-locate ()
  "When anvil-server is not on `load-path' the warning flags it."
  (cl-letf (((symbol-function 'locate-library) (lambda (&rest _) nil)))
    (let* ((anvil-dev-source-path nil)
           (res (anvil-self-sync-check)))
      (should (null (plist-get res :installed-dir)))
      (should (string-match-p "not located" (plist-get res :warning))))))

(ert-deftest anvil-dev-test-self-sync-check-no-dev-path-is-in-sync ()
  "Without `anvil-dev-source-path' we compare nothing, so :in-sync t."
  (let ((installed (anvil-dev-test--make-dir)))
    (unwind-protect
        (let ((anvil-dev-source-path nil))
          (cl-letf (((symbol-function 'locate-library)
                     (lambda (&rest _)
                       (expand-file-name "anvil-server.el" installed))))
            (anvil-dev-test--with-git
                (list
                 (cons (lambda (args) (member "--porcelain" args)) "")
                 (cons (anvil-dev-test--args-has "--abbrev-ref") "master")
                 (cons (anvil-dev-test--args-has "HEAD") "ccccccc"))
              (let ((res (anvil-self-sync-check)))
                (should (eq t (plist-get res :in-sync)))
                (should (null (plist-get res :dev-head)))
                (should (null (plist-get res :warning)))))))
      (delete-directory installed t))))

;;;; --- anvil-journal-append -----------------------------------------------

(defun anvil-dev-test--with-journal (initial-content proc)
  "Write INITIAL-CONTENT to a temp org file, call PROC with its path.
PROC is called once; the file and any visiting buffer are cleaned up."
  (let* ((f (make-temp-file "anvil-journal-" nil ".org"))
         (buf nil))
    (unwind-protect
        (progn
          (let ((coding-system-for-write 'utf-8-unix))
            (write-region initial-content nil f nil 'silent))
          (funcall proc f)
          (setq buf (get-file-buffer f)))
      (when (and buf (buffer-live-p buf)) (kill-buffer buf))
      (ignore-errors (delete-file f)))))

(defun anvil-dev-test--slurp (path)
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8))
      (insert-file-contents path))
    (buffer-string)))

(ert-deftest anvil-dev-test-journal-append-creates-worklog-section ()
  "Inserting under a day with no worklog section creates it + MEMO."
  (anvil-dev-test--with-journal
   "* 2026-04-16-Thursday\n** ROKUYO 赤口 <2026-04-16 Thu>\n"
   (lambda (f)
     (let ((res (anvil-journal-append f "2026-04-16" "smoke test" "body\n")))
       (should (eq t (plist-get res :section-created)))
       (should (> (plist-get res :bytes-inserted) 0)))
     (let ((text (anvil-dev-test--slurp f)))
       (should (string-match-p
                "^\\*\\* NOTE 作業ログ <2026-04-16 Thu>" text))
       (should (string-match-p
                "^\\*\\*\\* MEMO AI: smoke test <2026-04-16 Thu>" text))
       (should (string-match-p "^body$" text))))))

(ert-deftest anvil-dev-test-journal-append-reuses-existing-worklog ()
  "A second MEMO goes under the existing 作業ログ section, not a new one."
  (anvil-dev-test--with-journal
   (concat "* 2026-04-16-Thursday\n"
           "** NOTE 作業ログ <2026-04-16 Thu>\n"
           "*** MEMO AI: first <2026-04-16 Thu>\nFirst body.\n")
   (lambda (f)
     (let ((res (anvil-journal-append f "2026-04-16" "second"
                                      "second body\n")))
       (should (null (plist-get res :section-created))))
     (let* ((text (anvil-dev-test--slurp f))
            ;; Count of 作業ログ headings should still be exactly 1.
            (section-count
             (length (split-string text "^\\*\\* NOTE 作業ログ" t))))
       (should (= 2 section-count))  ; split gives N+1 pieces
       (should (string-match-p
                "^\\*\\*\\* MEMO AI: first" text))
       (should (string-match-p
                "^\\*\\*\\* MEMO AI: second" text))
       ;; second MEMO must appear AFTER first.
       (let ((first-pos (string-match "MEMO AI: first" text))
             (second-pos (string-match "MEMO AI: second" text)))
         (should (and first-pos second-pos (< first-pos second-pos))))))))

(ert-deftest anvil-dev-test-journal-append-preserves-other-sections ()
  "Existing NOTE / MEMO entries in earlier days stay untouched."
  (anvil-dev-test--with-journal
   (concat "* 2026-04-15-Wednesday\n"
           "** NOTE 作業ログ <2026-04-15 Wed>\n"
           "*** MEMO AI: past <2026-04-15 Wed>\nPast body.\n\n"
           "* 2026-04-16-Thursday\n")
   (lambda (f)
     (anvil-journal-append f "2026-04-16" "today" "Today body.\n")
     (let ((text (anvil-dev-test--slurp f)))
       (should (string-match-p "MEMO AI: past" text))
       (should (string-match-p "Past body\\." text))
       (should (string-match-p "MEMO AI: today" text))
       ;; The new 作業ログ section goes under the 2026-04-16 heading.
       (let ((today-pos (string-match "^\\* 2026-04-16-Thursday" text))
             (new-section-pos (string-match
                               "^\\*\\* NOTE 作業ログ <2026-04-16" text)))
         (should (and today-pos new-section-pos (< today-pos new-section-pos))))))))

(ert-deftest anvil-dev-test-journal-append-errors-on-missing-day ()
  "Appending under a date with no level-1 heading signals `user-error'."
  (anvil-dev-test--with-journal
   "* 2026-04-15-Wednesday\n"
   (lambda (f)
     (should-error (anvil-journal-append f "2026-04-20"
                                         "never" "whatever\n")
                   :type 'error))))

(ert-deftest anvil-dev-test-weekday-abbrev-matches-org ()
  "Helper matches org's 3-letter weekday convention."
  (should (equal "Thu" (anvil-dev--weekday-abbrev "2026-04-16")))
  (should (equal "Sun" (anvil-dev--weekday-abbrev "2026-04-19"))))

;;; anvil-dev-test.el ends here
