;;; anvil-elisp-test.el --- Tests for anvil-elisp -*- lexical-binding: t; -*-

;;; Commentary:

;; Phase B2 (Doc 38) — verify that anvil-elisp's 6 portable MCP tools
;; work in both Emacs and NeLisp runtimes.
;;
;; The 7th tool (`elisp-info-lookup-symbol') was moved to
;; `anvil-ide-elisp.el' in Phase C and is exercised by
;; `tests/anvil-ide-elisp-test.el'.
;;
;; The split-out fallback / portable-renderer paths are exercised via
;; `cl-letf' so the tests do not require an actual NeLisp runtime.
;;
;; Also covers regression tests for `anvil-elisp--get-function-definition'
;; (native-comp file-backed dispatch, synthetic stub for native-comp
;; functions without a source file).

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'json)
(require 'anvil-elisp)

(defun anvil-elisp-test--parse-json (text)
  "Parse TEXT as a plist-shaped JSON object."
  (json-parse-string text
                     :object-type 'plist
                     :array-type 'list
                     :null-object nil
                     :false-object nil))

(defun anvil-elisp-test--require-native-compiler ()
  "Skip unless this runner can actually produce native-compiled functions."
  (unless (and (fboundp 'native-compile)
               (fboundp 'native-comp-function-p)
               (fboundp 'native-comp-available-p)
               (native-comp-available-p))
    (ert-skip "native compilation is unavailable on this runner")))

(defun anvil-elisp-test--native-compile-symbol-or-skip (sym)
  "Native-compile SYM or skip if the runner cannot do so reliably."
  (anvil-elisp-test--require-native-compiler)
  (condition-case err
      (let ((compiled (native-compile sym)))
        (unless (native-comp-function-p compiled)
          (ert-skip
           (format "native-compile did not return a native function for %s"
                   sym)))
        (fset sym compiled))
    (error
     (ert-skip
      (format "native compilation failed on this runner: %s"
              (error-message-string err))))))

(defun anvil-elisp-test--with-temp-source (fn)
  "Write a temporary Elisp fixture and call FN with its path."
  (let* ((dir (make-temp-file "anvil-elisp-test-" t))
         (file (expand-file-name "fixture.el" dir))
         (feature 'anvil-elisp-test-fixture)
         (symbols '(anvil-elisp-test-fixture-fn
                    anvil-elisp-test-fixture-alias)))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert ";;; fixture.el --- temp -*- lexical-binding: t; -*-\n\n"
                    ";; Header for temp source fn\n"
                    "(defun anvil-elisp-test-fixture-fn (x)\n"
                    "  \"Return X plus one.\"\n"
                    "  (+ x 1))\n\n"
                    "(defalias 'anvil-elisp-test-fixture-alias\n"
                    "  #'anvil-elisp-test-fixture-fn\n"
                    "  \"Alias doc.\")\n\n"
                    "(provide 'anvil-elisp-test-fixture)\n"))
          (load-file file)
          (funcall fn file))
      (ignore-errors
        (when (featurep feature)
          (unload-feature feature t)))
      (dolist (sym symbols)
        (ignore-errors (fmakunbound sym)))
      (when (file-exists-p dir)
        (delete-directory dir t)))))

;;;; --- elisp-describe-function: portable renderer -------------------

(ert-deftest anvil-elisp-test-describe-function-portable-uses-documentation ()
  "`anvil-elisp--describe-function-portable' returns a string that
contains the docstring and a function signature, without calling
`describe-function-1'."
  (let ((called-describe-1 nil))
    (cl-letf (((symbol-function 'describe-function-1)
               (lambda (&rest _) (setq called-describe-1 t))))
      (let ((out (anvil-elisp--describe-function "car")))
        (should (stringp out))
        (should (string-match-p "(car" out))
        (should-not called-describe-1)))))

(ert-deftest anvil-elisp-test-describe-function-void-throws ()
  "Void functions still raise `anvil-server-tool-error'."
  (should-error
   (anvil-elisp--describe-function
    "anvil-elisp-test--definitely-not-a-function")
   :type 'anvil-server-tool-error))

;;;; --- elisp-info-lookup-symbol moved to anvil-ide-elisp-test.el (Doc 38 Phase C)

;;;; --- elisp-byte-compile-file: nelisp-cc delegate ------------------

(defun anvil-elisp-test--write (path content)
  (let ((coding-system-for-write 'utf-8-unix))
    (write-region content nil path nil 'silent)))

(ert-deftest anvil-elisp-test-byte-compile-fallback-when-no-nelisp-cc ()
  "Without `nelisp-cc-runtime-compile-and-allocate' the original
`byte-compile-file' renderer runs and the `:backend' key is absent.
Skipped if NeLisp happens to be loaded into this Emacs."
  (skip-unless (not (fboundp 'nelisp-cc-runtime-compile-and-allocate)))
  (let ((tmp (make-temp-file "anvil-elisp-bc-" nil ".el")))
    (unwind-protect
        (progn
          (anvil-elisp-test--write
           tmp
           ";;; tmp --- x -*- lexical-binding: t; -*-
;;; Commentary: tmp
;;; Code:
(defun anvil-elisp-test-tmp-fn () \"clean.\" 1)
(provide 'tmp)
;;; tmp ends here
")
          (let* ((out (anvil-elisp--byte-compile-file tmp))
                 (res (car (read-from-string out))))
            (should (eq t (plist-get res :ok)))
            (should (null (plist-get res :backend)))))
      (ignore-errors (delete-file tmp))
      (ignore-errors
        (delete-file (concat (file-name-sans-extension tmp) ".elc"))))))

(ert-deftest anvil-elisp-test-byte-compile-uses-nelisp-cc-when-bound ()
  "When `nelisp-cc-runtime-compile-and-allocate' is bound the tool
delegates each top-level defun to it and tags `:backend nelisp-cc'."
  (let ((calls 0))
    (cl-letf (((symbol-function 'nelisp-cc-runtime-compile-and-allocate)
               (lambda (form &optional _backend)
                 (cl-incf calls)
                 ;; Pretend success.
                 (list :exec-page :stub :backend 'x86_64))))
      (let ((tmp (make-temp-file "anvil-elisp-bc-nelisp-" nil ".el")))
        (unwind-protect
            (progn
              (anvil-elisp-test--write
               tmp
               "(defun anvil-elisp-test-nfn-a () 1)
(defun anvil-elisp-test-nfn-b (x) (+ x 1))
(defvar anvil-elisp-test-nv 42)
")
              (let* ((out (anvil-elisp--byte-compile-file tmp))
                     (res (car (read-from-string out))))
                (should (eq t (plist-get res :ok)))
                (should (eq 'nelisp-cc (plist-get res :backend)))
                (should (= 2 (plist-get res :compiled-forms)))
                (should (= 2 calls))))
          (ignore-errors (delete-file tmp)))))))

(ert-deftest anvil-elisp-test-byte-compile-nelisp-captures-form-errors ()
  "A nelisp-cc error on one form is captured into :warnings, not
raised."
  (cl-letf (((symbol-function 'nelisp-cc-runtime-compile-and-allocate)
             (lambda (_form &optional _backend)
               (error "synthetic compile error"))))
    (let ((tmp (make-temp-file "anvil-elisp-bc-nelisp-err-" nil ".el")))
      (unwind-protect
          (progn
            (anvil-elisp-test--write
             tmp "(defun anvil-elisp-test-bad-fn () 1)\n")
            (let* ((out (anvil-elisp--byte-compile-file tmp))
                   (res (car (read-from-string out)))
                   (warns (plist-get res :warnings)))
              (should (eq t (plist-get res :ok)))
              (should (= 1 (length warns)))
              (should (string-match-p "synthetic compile error"
                                      (car warns)))))
        (ignore-errors (delete-file tmp))))))

;;;; --- elisp-get-function-definition: native-comp / dispatch ------

(ert-deftest anvil-elisp-test-get-function-definition-c-primitive ()
  "True C primitives still report the C-function shape."
  (let ((res (anvil-elisp-test--parse-json
              (anvil-elisp--get-function-definition "car"))))
    (should (plist-get res :is-c-function))
    (should (equal "car" (plist-get res :function-name)))))

(ert-deftest anvil-elisp-test-dispatch-prefers-source-file-over-subr ()
  "A file-backed `subr' must go through the source-file path."
  (cl-letf (((symbol-function 'find-lisp-object-file-name)
             (lambda (_sym _type) "/tmp/fake-source.el"))
            ((symbol-function 'anvil-elisp--get-function-definition-from-file)
             (lambda (function sym func-file is-alias aliased-to)
               (list :from-file function sym func-file is-alias aliased-to)))
            ((symbol-function 'anvil-elisp--get-function-definition-c-function)
             (lambda (&rest _args)
               :c-function)))
    (should
     (equal
      '(:from-file "car" car "/tmp/fake-source.el" nil nil)
      (anvil-elisp--get-function-definition-dispatch
       "car" 'car (list (symbol-function 'car) nil nil))))))

(ert-deftest anvil-elisp-test-native-compiled-file-backed-function-uses-source ()
  "Native-compiled Elisp functions with a source file must not be treated as C."
  (anvil-elisp-test--require-native-compiler)
  (anvil-elisp-test--with-temp-source
   (lambda (file)
     (anvil-elisp-test--native-compile-symbol-or-skip
      'anvil-elisp-test-fixture-fn)
     (should (subrp (symbol-function 'anvil-elisp-test-fixture-fn)))
     (should-not
      (and (fboundp 'subr-primitive-p)
           (subr-primitive-p (symbol-function 'anvil-elisp-test-fixture-fn))))
     (let ((res (anvil-elisp-test--parse-json
                 (anvil-elisp--get-function-definition
                  "anvil-elisp-test-fixture-fn"))))
       (should-not (plist-member res :is-c-function))
       (should (equal file (plist-get res :file-path)))
       (should
        (string-match-p
         "(defun anvil-elisp-test-fixture-fn"
         (plist-get res :source)))))))

(ert-deftest anvil-elisp-test-native-compiled-no-source-returns-synthetic-stub ()
  "A native-compiled function without source should return a synthetic stub."
  (anvil-elisp-test--require-native-compiler)
  (let ((sym 'anvil-elisp-test-native-no-source))
    (unwind-protect
        (progn
          (defalias sym
            (lambda (x y)
              "Standalone native doc."
              (+ x y 3)))
          (anvil-elisp-test--native-compile-symbol-or-skip sym)
          (let ((res (anvil-elisp-test--parse-json
                      (anvil-elisp--get-function-definition
                       (symbol-name sym)))))
            (should-not (plist-member res :is-c-function))
            (should (plist-get res :source-unavailable))
            (should (equal "native-compiled-no-source"
                           (plist-get res :reason)))
            (should (equal "<native-compiled>"
                           (plist-get res :file-path)))
            (should (string-match-p
                     "Synthetic stub"
                     (plist-get res :source)))
            (should (string-match-p
                     "(defun anvil-elisp-test-native-no-source"
                     (plist-get res :source)))
            (should (string-match-p
                     "Standalone native doc."
                     (plist-get res :source)))))
      (ignore-errors (fmakunbound sym)))))

(provide 'anvil-elisp-test)
;;; anvil-elisp-test.el ends here
