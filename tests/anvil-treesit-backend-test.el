;;; anvil-treesit-backend-test.el --- Tests for anvil-treesit-backend  -*- lexical-binding: t; -*-

;;; Commentary:

;; Doc 38 Phase F backend dispatch tests + Phase G subprocess backend
;; tests.  Phase F tests are pure (no subprocess); they verify
;; `anvil-treesit-backend-pick' under each policy and the structured-
;; error shape returned by the still-stubbed `with-root' / etc paths.
;; Phase G tests guard with `skip-unless' on tool availability so
;; `make test-all' stays fast / hermetic on minimal hosts.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'anvil-treesit-backend nil 'noerror)

(defun anvil-treesit-backend-test--loaded-p ()
  "Return non-nil when the backend module is loaded."
  (fboundp 'anvil-treesit-backend-pick))

(defun anvil-treesit-backend-test--has-python ()
  "Return non-nil when the subprocess backend can shell out to python3."
  (and (anvil-treesit-backend-test--loaded-p)
       (anvil-treesit-backend--find-python)))

(defun anvil-treesit-backend-test--has-acorn ()
  "Return non-nil when the subprocess backend can shell out to acorn."
  (and (anvil-treesit-backend-test--loaded-p)
       (anvil-treesit-backend--find-acorn)))

(defun anvil-treesit-backend-test--err-plist (err)
  "Round-trip the data of a `user-error' ERR through `read'.
The subprocess backend always formats its structured error data
through `format \"%S\"', so we recover the plist by reading the
message back."
  (let* ((data (cdr err))
         (msg (if (consp data) (car data) data)))
    (read msg)))

;;;; --- backend pick dispatch ---------------------------------------------

(ert-deftest anvil-treesit-backend-pick-honours-treesit-policy ()
  "Forcing `treesit' returns nil when grammar absent, the symbol when present."
  (skip-unless (anvil-treesit-backend-test--loaded-p))
  (let ((anvil-treesit-backend-preferred 'treesit))
    (cond
     ((and (fboundp 'treesit-language-available-p)
           (treesit-language-available-p 'python))
      (should (eq 'treesit (anvil-treesit-backend-pick 'python))))
     (t
      (should (eq nil (anvil-treesit-backend-pick 'python)))))))

(ert-deftest anvil-treesit-backend-pick-honours-subprocess-policy ()
  "Forcing `subprocess' always returns the symbol regardless of treesit state."
  (skip-unless (anvil-treesit-backend-test--loaded-p))
  (let ((anvil-treesit-backend-preferred 'subprocess))
    (should (eq 'subprocess (anvil-treesit-backend-pick 'python)))
    (should (eq 'subprocess (anvil-treesit-backend-pick 'typescript)))
    (should (eq 'subprocess (anvil-treesit-backend-pick 'javascript)))))

(ert-deftest anvil-treesit-backend-pick-auto-falls-back-to-subprocess ()
  "Auto policy picks treesit when grammar present, subprocess otherwise."
  (skip-unless (anvil-treesit-backend-test--loaded-p))
  (let ((anvil-treesit-backend-preferred 'auto))
    ;; A bogus language symbol is never treesit-available; auto must
    ;; fall through to the subprocess stub.
    (should (eq 'subprocess
                (anvil-treesit-backend-pick 'utterly-fake-lang-zzz)))))

;;;; --- with-root still stubbed under subprocess (Phase G TODO) ----------

(ert-deftest anvil-treesit-backend-with-root-on-subprocess-stubs ()
  "`anvil-treesit-with-root' under the subprocess backend signals the stub.
Phase G ships parse/query/node-text/node-range; with-root remains a
TODO until file IO + parser tear-down are wired through subprocess."
  (skip-unless (anvil-treesit-backend-test--loaded-p))
  (let ((anvil-treesit-backend-preferred 'subprocess)
        (tmp (make-temp-file "anvil-treesit-backend-test-" nil ".py")))
    (unwind-protect
        (progn
          (with-temp-file tmp (insert "x = 1\n"))
          (should-error
           (anvil-treesit-with-root tmp 'python _root nil)
           :type 'user-error))
      (delete-file tmp))))

;;;; --- Phase G: subprocess parse / query / node-text / node-range -------

(ert-deftest anvil-treesit-backend-subprocess-parse-python ()
  "Parsing Python source returns a `Module' root with FunctionDef child."
  (skip-unless (anvil-treesit-backend-test--has-python))
  (let* ((src "x = 1\ny = 2\ndef foo(a, b):\n    return a + b\n")
         (ast (anvil-treesit-backend-subprocess-parse src 'python)))
    (should (equal "Module" (plist-get ast :type)))
    (should (eq 'python (plist-get ast :lang)))
    (should (eq 'subprocess (plist-get ast :backend)))
    (should (>= (length (plist-get ast :children)) 1))
    (let ((funcs (anvil-treesit-backend-subprocess-query ast "FunctionDef")))
      (should (= 1 (length funcs)))
      (should (equal "foo" (plist-get (car funcs) :name))))))

(ert-deftest anvil-treesit-backend-subprocess-parse-javascript ()
  "Parsing JS via acorn returns a `Program' root with FunctionDeclaration."
  (skip-unless (anvil-treesit-backend-test--has-acorn))
  (let* ((src "function add(a, b) { return a + b; }\nlet x = add(1, 2);\n")
         (ast (anvil-treesit-backend-subprocess-parse src 'javascript)))
    (should (equal "Program" (plist-get ast :type)))
    (should (eq 'javascript (plist-get ast :lang)))
    (let ((fns (anvil-treesit-backend-subprocess-query
                ast "FunctionDeclaration")))
      (should (>= (length fns) 1))
      (should (equal "add" (plist-get (car fns) :name))))))

(ert-deftest anvil-treesit-backend-subprocess-parse-typescript-degraded ()
  "Parsing TypeScript falls back to acorn (= JS subset, no type checks)."
  (skip-unless (anvil-treesit-backend-test--has-acorn))
  (let* ((src "function add(a, b) { return a + b; }\n")
         (ast (anvil-treesit-backend-subprocess-parse src 'typescript)))
    (should (equal "Program" (plist-get ast :type)))
    (should (eq 'typescript (plist-get ast :lang)))))

(ert-deftest anvil-treesit-backend-subprocess-query-by-plist-selector ()
  "Query accepts both string node-type and plist `(:type ...)' selector."
  (skip-unless (anvil-treesit-backend-test--has-python))
  (let* ((ast (anvil-treesit-backend-subprocess-parse
               "def a(): pass\ndef b(): pass\n" 'python)))
    (should (= 2 (length (anvil-treesit-backend-subprocess-query
                          ast "FunctionDef"))))
    (should (= 2 (length (anvil-treesit-backend-subprocess-query
                          ast '(:type "FunctionDef")))))
    (should (= 0 (length (anvil-treesit-backend-subprocess-query
                          ast "NoSuchType"))))))

(ert-deftest anvil-treesit-backend-subprocess-node-text-and-range ()
  "node-text slices SOURCE on (:start . :end); node-range cons-pairs them."
  (skip-unless (anvil-treesit-backend-test--has-python))
  (let* ((src "x = 1\ny = 2\ndef foo(a, b):\n    return a + b\n")
         (ast (anvil-treesit-backend-subprocess-parse src 'python))
         (f (car (anvil-treesit-backend-subprocess-query ast "FunctionDef"))))
    (let ((r (anvil-treesit-backend-subprocess-node-range f)))
      (should (consp r))
      (should (integerp (car r)))
      (should (integerp (cdr r)))
      (should (< (car r) (cdr r))))
    (let ((txt (anvil-treesit-backend-subprocess-node-text f src)))
      (should (stringp txt))
      (should (string-match-p "\\`def foo" txt))
      (should (string-match-p "return a \\+ b\\'" txt)))))

(ert-deftest anvil-treesit-backend-subprocess-empty-source-python ()
  "Parsing an empty Python source returns a Module with no children."
  (skip-unless (anvil-treesit-backend-test--has-python))
  (let ((ast (anvil-treesit-backend-subprocess-parse "" 'python)))
    (should (equal "Module" (plist-get ast :type)))
    (should (null (plist-get ast :children)))))

(ert-deftest anvil-treesit-backend-subprocess-empty-source-javascript ()
  "Parsing an empty JavaScript source returns a Program with no children."
  (skip-unless (anvil-treesit-backend-test--has-acorn))
  (let ((ast (anvil-treesit-backend-subprocess-parse "" 'javascript)))
    (should (equal "Program" (plist-get ast :type)))
    (should (null (plist-get ast :children)))))

(ert-deftest anvil-treesit-backend-subprocess-tool-not-found-python ()
  "When python3 is absent from PATH, parse signals `:tool-not-found'."
  (skip-unless (anvil-treesit-backend-test--loaded-p))
  ;; Strip PATH and override the auto-detect customization to nil so
  ;; `--find-python' returns nil.
  (let ((process-environment (list "PATH=/nonexistent-anvil-test"))
        (exec-path (list "/nonexistent-anvil-test"))
        (anvil-treesit-backend-subprocess-python-executable nil))
    (let* ((err (should-error
                 (anvil-treesit-backend-subprocess-parse "x = 1" 'python)
                 :type 'user-error))
           (p (anvil-treesit-backend-test--err-plist err)))
      (should (eq :tool-not-found (plist-get p :status)))
      (should (eq 'python (plist-get p :lang)))
      (should (equal "python3" (plist-get p :tool)))
      (should (stringp (plist-get p :hint))))))

(ert-deftest anvil-treesit-backend-subprocess-tool-not-found-javascript ()
  "When acorn is absent from PATH, JS parse signals `:tool-not-found'."
  (skip-unless (anvil-treesit-backend-test--loaded-p))
  (let ((process-environment (list "PATH=/nonexistent-anvil-test"))
        (exec-path (list "/nonexistent-anvil-test"))
        (anvil-treesit-backend-subprocess-acorn-executable nil))
    (let* ((err (should-error
                 (anvil-treesit-backend-subprocess-parse "var x = 1" 'javascript)
                 :type 'user-error))
           (p (anvil-treesit-backend-test--err-plist err)))
      (should (eq :tool-not-found (plist-get p :status)))
      (should (eq 'javascript (plist-get p :lang)))
      (should (equal "acorn" (plist-get p :tool))))))

(ert-deftest anvil-treesit-backend-subprocess-unsupported-lang ()
  "An unknown language signals `:unsupported-lang'."
  (skip-unless (anvil-treesit-backend-test--loaded-p))
  (let* ((err (should-error
               (anvil-treesit-backend-subprocess-parse "x" 'cobol)
               :type 'user-error))
         (p (anvil-treesit-backend-test--err-plist err)))
    (should (eq :unsupported-lang (plist-get p :status)))
    (should (eq 'cobol (plist-get p :lang)))))

(ert-deftest anvil-treesit-backend-subprocess-parse-error-python ()
  "Syntactically invalid Python signals `:parse-error' with line info."
  (skip-unless (anvil-treesit-backend-test--has-python))
  (let* ((err (should-error
               (anvil-treesit-backend-subprocess-parse "def (" 'python)
               :type 'user-error))
         (p (anvil-treesit-backend-test--err-plist err)))
    (should (eq :parse-error (plist-get p :status)))
    (should (eq 'python (plist-get p :lang)))
    (should (equal "SyntaxError" (plist-get p :error)))))

(ert-deftest anvil-treesit-backend-subprocess-node-text-out-of-bounds ()
  "node-text returns nil when offsets are missing or out-of-bounds."
  (skip-unless (anvil-treesit-backend-test--loaded-p))
  ;; Hand-built node with no offsets.
  (should (null (anvil-treesit-backend-subprocess-node-text
                 '(:type "X") "abc")))
  ;; Out-of-bounds offsets.
  (should (null (anvil-treesit-backend-subprocess-node-text
                 '(:type "X" :start 10 :end 99) "abc"))))

(ert-deftest anvil-treesit-backend-subprocess-node-range-missing-offsets ()
  "node-range returns nil when either offset is missing."
  (skip-unless (anvil-treesit-backend-test--loaded-p))
  (should (null (anvil-treesit-backend-subprocess-node-range '(:type "X"))))
  (should (null (anvil-treesit-backend-subprocess-node-range
                 '(:type "X" :start 0)))))

(provide 'anvil-treesit-backend-test)
;;; anvil-treesit-backend-test.el ends here
