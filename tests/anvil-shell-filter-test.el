;;; anvil-shell-filter-test.el --- Tests for anvil-shell-filter -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT suite for Doc 27 Phase 1 — per-command shell output
;; compression + tee + gain statistics.  Each filter is a pure
;; string-to-string function so the unit tests feed canned raw
;; output and assert a shape-locked compressed form.
;;
;; Tests that need the `anvil-state' KV (tee round-trip, gain
;; accumulation) isolate themselves with a temp DB via
;; `anvil-shell-filter-test--with-state' so the real DB is never
;; touched.
;;
;; Every test gates itself with `skip-unless' on
;; `anvil-shell-filter-supported' membership so the TDD-lock commit
;; can land ahead of the impl without red CI.

;;; Code:

(require 'ert)
(require 'cl-lib)
;; Non-fatal: the TDD-lock commit lands before the module, so
;; `anvil-shell-filter' is intentionally absent on first load.
;; `skip-unless' below gates every real assertion on the
;; `anvil-shell-filter-supported' capability list and turns red
;; only once the impl declares it has landed.
(require 'anvil-shell-filter nil t)
(require 'anvil-state)


;;;; --- fixture helpers ----------------------------------------------------

(defmacro anvil-shell-filter-test--with-state (&rest body)
  "Run BODY with a fresh temp `anvil-state' DB."
  (declare (indent 0))
  `(let ((anvil-state-db-path (make-temp-file "anvil-shf-" nil ".db"))
         (anvil-state--db nil))
     (unwind-protect
         (progn (anvil-state-enable) ,@body)
       (anvil-state-disable)
       (ignore-errors (delete-file anvil-state-db-path)))))

(defun anvil-shell-filter-test--supported-p (tag)
  "Return non-nil when TAG is in `anvil-shell-filter-supported'."
  (and (boundp 'anvil-shell-filter-supported)
       (memq tag anvil-shell-filter-supported)))


;;;; --- per-command filter snapshots ---------------------------------------

(ert-deftest anvil-shell-filter-test/git-status-porcelain-summary ()
  "`git status --short --branch' output compresses to one-line summary."
  (skip-unless (anvil-shell-filter-test--supported-p 'git-status))
  (let* ((raw (concat
               "## main...origin/main [ahead 2]\n"
               " M anvil-file.el\n"
               " M anvil-git.el\n"
               "?? .worktrees/\n"
               "?? notes.org\n"
               "?? scratch.el\n"
               "A  anvil-shell-filter.el\n"
               "D  obsolete.el\n"))
         (out (anvil-shell-filter-apply 'git-status raw)))
    (should (stringp out))
    (should (string-match-p "\\`branch:main" out))
    (should (string-match-p "\\+2" out))
    (should (string-match-p "M:2" out))
    (should (string-match-p "\\?\\?:3" out))
    (should (string-match-p "A:1" out))
    (should (string-match-p "D:1" out))
    (should (< (length out) (length raw)))))

(ert-deftest anvil-shell-filter-test/git-status-detached-head ()
  "Detached-HEAD `## HEAD (no branch)' line still parses."
  (skip-unless (anvil-shell-filter-test--supported-p 'git-status))
  (let* ((raw "## HEAD (no branch)\n M foo.el\n")
         (out (anvil-shell-filter-apply 'git-status raw)))
    (should (string-match-p "\\`branch:" out))
    (should (string-match-p "M:1" out))))

(ert-deftest anvil-shell-filter-test/git-log-oneline ()
  "Verbose `git log' reduces to `hash subject' per commit."
  (skip-unless (anvil-shell-filter-test--supported-p 'git-log))
  (let* ((raw (concat
               "commit deadbeefcafebabe1234567890abcdef12345678\n"
               "Author: Zaw <z@example.com>\n"
               "Date:   Wed Apr 22 11:00:00 2026 +0900\n"
               "\n"
               "    feat(27): shell filter skeleton\n"
               "\n"
               "commit feedfacec001d00d9876543210abcdef87654321\n"
               "Author: Zaw <z@example.com>\n"
               "Date:   Wed Apr 22 10:30:00 2026 +0900\n"
               "\n"
               "    test(27): shape-lock filters\n"))
         (out (anvil-shell-filter-apply 'git-log raw)))
    (should (stringp out))
    (let ((lines (split-string out "\n" t)))
      (should (= 2 (length lines)))
      (should (string-match-p "^deadbee[a-f0-9]* feat(27): shell filter skeleton" (nth 0 lines)))
      (should (string-match-p "^feedfac[a-f0-9]* test(27): shape-lock filters" (nth 1 lines))))))

(ert-deftest anvil-shell-filter-test/git-diff-hunk-cap ()
  "Long unified diff keeps ≤3 hunks + omitted-hunks footer."
  (skip-unless (anvil-shell-filter-test--supported-p 'git-diff))
  (let* ((raw (mapconcat
               #'identity
               (append
                (list "diff --git a/foo.el b/foo.el"
                      "--- a/foo.el"
                      "+++ b/foo.el")
                (cl-loop for i from 1 to 6
                         append (list (format "@@ -%d,3 +%d,3 @@" i i)
                                      " context-before"
                                      "-old-line"
                                      "+new-line"
                                      " context-after")))
               "\n"))
         (out (anvil-shell-filter-apply 'git-diff raw)))
    (should (stringp out))
    ;; hunk-header count must be 3
    (let ((hunks 0)
          (start 0))
      (while (string-match "^@@ " out start)
        (setq hunks (1+ hunks)
              start (match-end 0)))
      (should (= 3 hunks)))
    (should (string-match-p "3 more hunks omitted" out))))

(ert-deftest anvil-shell-filter-test/git-diff-short-passthrough ()
  "Diff with ≤3 hunks is returned without omitted-hunks footer."
  (skip-unless (anvil-shell-filter-test--supported-p 'git-diff))
  (let* ((raw (concat "diff --git a/foo.el b/foo.el\n"
                      "@@ -1,1 +1,1 @@\n"
                      "-a\n"
                      "+b\n"))
         (out (anvil-shell-filter-apply 'git-diff raw)))
    (should (stringp out))
    (should-not (string-match-p "hunks omitted" out))))

(ert-deftest anvil-shell-filter-test/rg-grouped-by-file ()
  "rg output groups by file, caps at 3 matches, appends `(N more)'."
  (skip-unless (anvil-shell-filter-test--supported-p 'rg))
  (let* ((raw (concat
               "src/foo.el:10:  (foo 1)\n"
               "src/foo.el:20:  (foo 2)\n"
               "src/foo.el:30:  (foo 3)\n"
               "src/foo.el:40:  (foo 4)\n"
               "src/foo.el:50:  (foo 5)\n"
               "src/bar.el:5:   (foo 6)\n"))
         (out (anvil-shell-filter-apply 'rg raw)))
    (should (stringp out))
    (should (string-match-p "src/foo.el" out))
    (should (string-match-p "src/bar.el" out))
    (should (string-match-p "2 more" out))))

(ert-deftest anvil-shell-filter-test/find-grouped-by-dir ()
  "find output groups by directory with a count suffix."
  (skip-unless (anvil-shell-filter-test--supported-p 'find))
  (let* ((raw (concat
               "./src/a.el\n./src/b.el\n./src/c.el\n./src/d.el\n./src/e.el\n"
               "./tests/a-test.el\n./tests/b-test.el\n"))
         (out (anvil-shell-filter-apply 'find raw)))
    (should (stringp out))
    (should (string-match-p "./src/" out))
    (should (string-match-p "./tests/" out))
    (should (string-match-p "5" out))
    (should (string-match-p "2" out))))

(ert-deftest anvil-shell-filter-test/ls-counts ()
  "ls output collapses to counts when entries > threshold."
  (skip-unless (anvil-shell-filter-test--supported-p 'ls))
  (let* ((raw (concat
               (mapconcat (lambda (i) (format "file%02d.el" i))
                          (number-sequence 1 15) "\n")
               "\n"))
         (out (anvil-shell-filter-apply 'ls raw)))
    (should (stringp out))
    (should (string-match-p "15" out))))

(ert-deftest anvil-shell-filter-test/pytest-pass-count-only ()
  "Pytest passing run keeps summary line, drops verbose per-test output."
  (skip-unless (anvil-shell-filter-test--supported-p 'pytest))
  (let* ((raw (concat
               "============================= test session starts ==============================\n"
               "collected 42 items\n"
               "tests/test_a.py ..........                                                  [ 24%]\n"
               "tests/test_b.py ................                                            [ 62%]\n"
               "tests/test_c.py ................                                            [100%]\n"
               "\n"
               "============================== 42 passed in 1.23s ==============================\n"))
         (out (anvil-shell-filter-apply 'pytest raw)))
    (should (stringp out))
    (should (string-match-p "42 passed" out))
    (should (< (length out) (length raw)))))

(ert-deftest anvil-shell-filter-test/pytest-fail-keeps-traceback ()
  "Pytest failing run preserves the FAILED block + traceback."
  (skip-unless (anvil-shell-filter-test--supported-p 'pytest))
  (let* ((raw (concat
               "tests/test_a.py::test_ok PASSED\n"
               "tests/test_b.py::test_bad FAILED\n"
               "=================================== FAILURES ===================================\n"
               "________________________________ test_bad _____________________________________\n"
               "    def test_bad():\n"
               ">       assert 1 == 2\n"
               "E       assert 1 == 2\n"
               "tests/test_b.py:3: AssertionError\n"
               "========================= 1 failed, 1 passed in 0.01s =========================\n"))
         (out (anvil-shell-filter-apply 'pytest raw)))
    (should (string-match-p "FAILED" out))
    (should (string-match-p "test_bad" out))
    (should (string-match-p "AssertionError" out))))

(ert-deftest anvil-shell-filter-test/ert-batch-pass-count ()
  "ERT batch pass-only run reduces to summary."
  (skip-unless (anvil-shell-filter-test--supported-p 'ert-batch))
  (let* ((raw (concat
               "Running 10 tests (2026-04-22 12:00:00, selector `t')\n"
               "   passed  1/10  anvil-shell-filter-test/git-status\n"
               "   passed  2/10  anvil-shell-filter-test/git-log\n"
               "   passed  3/10  anvil-shell-filter-test/git-diff\n"
               "   passed  4/10  anvil-shell-filter-test/rg\n"
               "   passed  5/10  anvil-shell-filter-test/find\n"
               "   passed  6/10  anvil-shell-filter-test/ls\n"
               "   passed  7/10  anvil-shell-filter-test/pytest\n"
               "   passed  8/10  anvil-shell-filter-test/emacs-batch\n"
               "   passed  9/10  anvil-shell-filter-test/make\n"
               "   passed 10/10  anvil-shell-filter-test/ert-batch\n"
               "\n"
               "Ran 10 tests, 10 results as expected, 0 unexpected (0.123456 sec)\n"))
         (out (anvil-shell-filter-apply 'ert-batch raw)))
    (should (string-match-p "Ran 10 tests" out))
    (should (< (length out) (length raw)))))

(ert-deftest anvil-shell-filter-test/ert-batch-fail-keeps-detail ()
  "ERT batch with failures keeps the FAILED blocks."
  (skip-unless (anvil-shell-filter-test--supported-p 'ert-batch))
  (let* ((raw (concat
               "Running 2 tests\n"
               "   passed  1/2  t-ok\n"
               "   FAILED  2/2  t-bad\n"
               "Test t-bad condition:\n"
               "    (ert-test-failed\n"
               "     ((should (equal 1 2)) :form (equal 1 2) :value nil))\n"
               "\n"
               "Ran 2 tests, 1 results as expected, 1 unexpected\n"))
         (out (anvil-shell-filter-apply 'ert-batch raw)))
    (should (string-match-p "FAILED" out))
    (should (string-match-p "t-bad" out))
    (should (string-match-p "ert-test-failed" out))))

(ert-deftest anvil-shell-filter-test/emacs-batch-compile-squash ()
  "emacs --batch byte-compile output squashes `Compiling ...done' lines."
  (skip-unless (anvil-shell-filter-test--supported-p 'emacs-batch))
  (let* ((raw (concat
               "Compiling /tmp/a.el...\n"
               "Compiling /tmp/a.el...done\n"
               "Compiling /tmp/b.el...\n"
               "Compiling /tmp/b.el...done\n"
               "Compiling /tmp/c.el...\n"
               "Warning: foo bar\n"
               "Compiling /tmp/c.el...done\n"
               "gcs-done\n"))
         (out (anvil-shell-filter-apply 'emacs-batch raw)))
    (should (string-match-p "Warning:" out))
    (should-not (string-match-p "gcs-done" out))
    (should (< (length out) (length raw)))))

(ert-deftest anvil-shell-filter-test/make-keeps-errors ()
  "make output keeps error/warning lines and the final summary."
  (skip-unless (anvil-shell-filter-test--supported-p 'make))
  (let* ((raw (concat
               "cc -c foo.c\n"
               "foo.c:10:5: warning: unused variable\n"
               "cc -c bar.c\n"
               "cc -o prog foo.o bar.o\n"
               "make: *** [Makefile:15: prog] Error 1\n"))
         (out (anvil-shell-filter-apply 'make raw)))
    (should (string-match-p "warning" out))
    (should (string-match-p "Error 1" out))))


;;;; --- dispatch / lookup --------------------------------------------------

(ert-deftest anvil-shell-filter-test/lookup-by-first-token ()
  "`anvil-shell-filter-lookup' maps a command string to a handler tag."
  (skip-unless (anvil-shell-filter-test--supported-p 'dispatch))
  (should (eq 'git-status (anvil-shell-filter-lookup "git status --short")))
  (should (eq 'git-log    (anvil-shell-filter-lookup "git log --oneline -5")))
  (should (eq 'git-diff   (anvil-shell-filter-lookup "git diff HEAD~1")))
  (should (eq 'rg         (anvil-shell-filter-lookup "rg --no-heading foo")))
  (should (null (anvil-shell-filter-lookup "totally-unknown-cmd"))))

(ert-deftest anvil-shell-filter-test/apply-nil-filter-passthrough ()
  "A nil filter tag returns RAW unchanged."
  (skip-unless (anvil-shell-filter-test--supported-p 'dispatch))
  (let ((raw "random text\nsecond line\n"))
    (should (equal raw (anvil-shell-filter-apply nil raw)))))


;;;; --- tee round-trip + gain stats ---------------------------------------

(ert-deftest anvil-shell-filter-test/tee-roundtrip ()
  "`anvil-shell-filter--tee-put' stores raw; `tee-get' retrieves it."
  (skip-unless (anvil-shell-filter-test--supported-p 'tee))
  (anvil-shell-filter-test--with-state
    (let* ((raw "raw bytes\nline 2\n")
           (id (anvil-shell-filter--tee-put raw)))
      (should (stringp id))
      (should (equal raw (anvil-shell-filter-tee-get id))))))

(ert-deftest anvil-shell-filter-test/tee-get-missing-returns-nil ()
  "Unknown tee-id returns nil, never errors."
  (skip-unless (anvil-shell-filter-test--supported-p 'tee))
  (anvil-shell-filter-test--with-state
    (should (null (anvil-shell-filter-tee-get "no-such-id")))))

(ert-deftest anvil-shell-filter-test/gain-accumulates ()
  "`anvil-shell-filter--gain-record' accumulates savings; `gain' summarises."
  (skip-unless (anvil-shell-filter-test--supported-p 'gain))
  (anvil-shell-filter-test--with-state
    (anvil-shell-filter--gain-record 'git-status 3000 600)
    (anvil-shell-filter--gain-record 'git-diff   10000 2500)
    (let ((summary (anvil-shell-filter-gain)))
      (should (plist-get summary :entries))
      (should (>= (plist-get summary :entries) 2))
      (should (= 13000 (plist-get summary :raw-total)))
      (should (= 3100  (plist-get summary :compressed-total)))
      (should (= 9900  (plist-get summary :saved-total))))))


(provide 'anvil-shell-filter-test)

;;; anvil-shell-filter-test.el ends here
