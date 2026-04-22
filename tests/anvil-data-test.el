;;; anvil-data-test.el --- Tests for anvil-data Doc 33 Phase 1 -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT suite for Doc 33 Phase 1 — JSON path-based get / set / delete /
;; list-keys.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'anvil-data)


;;;; --- helpers ------------------------------------------------------------

(defmacro anvil-data-test--with-tmp-json (binding json-string &rest body)
  "Bind BINDING to a temp file containing JSON-STRING.
BINDING form: (FILE-VAR).  The file is deleted after BODY runs."
  (declare (indent 2))
  (let ((file-var (car binding)))
    `(let ((,file-var (make-temp-file "anvil-data-test-" nil ".json")))
       (unwind-protect
           (progn
             (with-temp-file ,file-var
               (insert ,json-string))
             ,@body)
         (when (file-exists-p ,file-var)
           (delete-file ,file-var))))))


;;;; --- path parser --------------------------------------------------------

(ert-deftest anvil-data-test-parse-path-empty-is-root ()
  (should (null (anvil-data--parse-path "")))
  (should (null (anvil-data--parse-path nil))))

(ert-deftest anvil-data-test-parse-path-single-ident ()
  (should (equal '((:object :foo))
                 (anvil-data--parse-path "foo"))))

(ert-deftest anvil-data-test-parse-path-dotted ()
  (should (equal '((:object :a) (:object :b) (:object :c))
                 (anvil-data--parse-path "a.b.c"))))

(ert-deftest anvil-data-test-parse-path-with-array-indices ()
  (should (equal '((:object :items) (:array 0) (:object :name))
                 (anvil-data--parse-path "items[0].name")))
  (should (equal '((:object :a) (:array 0) (:array 1))
                 (anvil-data--parse-path "a[0][1]"))))

(ert-deftest anvil-data-test-parse-path-rejects-malformed ()
  (should-error (anvil-data--parse-path "a..b"))
  (should-error (anvil-data--parse-path "a."))
  (should-error (anvil-data--parse-path "[abc]"))
  (should-error (anvil-data--parse-path "a[")))


;;;; --- get-path -----------------------------------------------------------

(ert-deftest anvil-data-test-get-path-shallow ()
  (anvil-data-test--with-tmp-json (f) "{\"name\": \"alice\", \"age\": 30}"
    (should (equal "alice" (anvil-data-path-get f "name")))
    (should (= 30 (anvil-data-path-get f "age")))))

(ert-deftest anvil-data-test-get-path-nested ()
  (anvil-data-test--with-tmp-json (f)
      "{\"a\":{\"b\":{\"c\":42}}}"
    (should (= 42 (anvil-data-path-get f "a.b.c")))))

(ert-deftest anvil-data-test-get-path-array-index ()
  (anvil-data-test--with-tmp-json (f)
      "{\"items\":[{\"name\":\"x\"},{\"name\":\"y\"}]}"
    (should (equal "y" (anvil-data-path-get f "items[1].name")))))

(ert-deftest anvil-data-test-get-path-missing-returns-nil ()
  (anvil-data-test--with-tmp-json (f) "{\"a\": 1}"
    (should (null (anvil-data-path-get f "b")))
    (should (null (anvil-data-path-get f "a.deep.nope")))))

(ert-deftest anvil-data-test-get-path-empty-returns-whole-tree ()
  (anvil-data-test--with-tmp-json (f) "{\"x\": 1}"
    (should (equal '(:x 1) (anvil-data-path-get f "")))))


;;;; --- set-path -----------------------------------------------------------

(ert-deftest anvil-data-test-set-path-overwrite-existing-leaf ()
  (anvil-data-test--with-tmp-json (f) "{\"a\": 1, \"b\": 2}"
    (let ((result (anvil-data-path-set f "a" 99 :apply t)))
      (should (plist-get result :applied))
      (should (= 99 (anvil-data-path-get f "a")))
      (should (= 2 (anvil-data-path-get f "b"))))))

(ert-deftest anvil-data-test-set-path-creates-deep-intermediates ()
  (anvil-data-test--with-tmp-json (f) "{}"
    (anvil-data-path-set f "a.b.c" "leaf" :apply t)
    (should (equal "leaf" (anvil-data-path-get f "a.b.c")))))

(ert-deftest anvil-data-test-set-path-preview-does-not-write ()
  (anvil-data-test--with-tmp-json (f) "{\"x\": 1}"
    (let ((before (anvil-data-path-get f "x"))
          (result (anvil-data-path-set f "x" 999)))
      (should-not (plist-get result :applied))
      (should (string-match-p "999" (plist-get result :preview)))
      (should (= 1 (anvil-data-path-get f "x")))
      (should (= 1 before)))))

(ert-deftest anvil-data-test-set-path-array-append-allowed ()
  (anvil-data-test--with-tmp-json (f) "{\"xs\":[1,2]}"
    (anvil-data-path-set f "xs[2]" 3 :apply t)
    (should (= 3 (anvil-data-path-get f "xs[2]")))))

(ert-deftest anvil-data-test-set-path-array-out-of-range-errors ()
  (anvil-data-test--with-tmp-json (f) "{\"xs\":[1]}"
    (should-error (anvil-data-path-set f "xs[5]" 9 :apply t))))

(ert-deftest anvil-data-test-set-path-type-mismatch-errors ()
  (anvil-data-test--with-tmp-json (f) "{\"a\":\"string-not-object\"}"
    (should-error (anvil-data-path-set f "a.b" 1 :apply t))))


;;;; --- delete-path --------------------------------------------------------

(ert-deftest anvil-data-test-delete-path-leaf ()
  (anvil-data-test--with-tmp-json (f) "{\"a\":1,\"b\":2}"
    (anvil-data-path-delete f "a" :apply t)
    (should (null (anvil-data-path-get f "a")))
    (should (= 2 (anvil-data-path-get f "b")))))

(ert-deftest anvil-data-test-delete-path-nested ()
  (anvil-data-test--with-tmp-json (f) "{\"a\":{\"b\":1,\"c\":2}}"
    (anvil-data-path-delete f "a.b" :apply t)
    (should (null (anvil-data-path-get f "a.b")))
    (should (= 2 (anvil-data-path-get f "a.c")))))

(ert-deftest anvil-data-test-delete-path-noop-on-missing ()
  (anvil-data-test--with-tmp-json (f) "{\"a\":1}"
    (let ((result (anvil-data-path-delete f "nope" :apply t)))
      (should (plist-get result :noop))
      (should-not (plist-get result :applied)))))

(ert-deftest anvil-data-test-delete-path-array-element ()
  (anvil-data-test--with-tmp-json (f) "{\"xs\":[10,20,30]}"
    (anvil-data-path-delete f "xs[1]" :apply t)
    (should (= 10 (anvil-data-path-get f "xs[0]")))
    (should (= 30 (anvil-data-path-get f "xs[1]")))))


;;;; --- list-keys ----------------------------------------------------------

(ert-deftest anvil-data-test-list-keys-root ()
  (anvil-data-test--with-tmp-json (f) "{\"a\":1,\"b\":2,\"c\":3}"
    (should (equal '("a" "b" "c") (anvil-data-path-keys f)))))

(ert-deftest anvil-data-test-list-keys-nested ()
  (anvil-data-test--with-tmp-json (f) "{\"a\":{\"x\":1,\"y\":2}}"
    (should (equal '("x" "y") (anvil-data-path-keys f "a")))))

(ert-deftest anvil-data-test-list-keys-array-returns-indices ()
  (anvil-data-test--with-tmp-json (f) "{\"xs\":[10,20,30]}"
    (should (equal '("0" "1" "2") (anvil-data-path-keys f "xs")))))

(ert-deftest anvil-data-test-list-keys-missing-path-errors ()
  (anvil-data-test--with-tmp-json (f) "{\"a\":1}"
    (should-error (anvil-data-path-keys f "nope"))))


;;;; --- format detection ---------------------------------------------------

(ert-deftest anvil-data-test-detect-format-defaults-to-json ()
  (should (eq 'json (anvil-data--detect-format "x.json")))
  (should (eq 'json (anvil-data--detect-format "x.json5")))
  (should (eq 'yaml (anvil-data--detect-format "x.yaml")))
  (should (eq 'yaml (anvil-data--detect-format "x.yml")))
  (should (eq 'toml (anvil-data--detect-format "x.toml")))
  (should (eq 'json (anvil-data--detect-format "noextension"))))

(ert-deftest anvil-data-test-non-json-format-errors-in-phase-1 ()
  (let ((tmp (make-temp-file "anvil-data-test-" nil ".yaml")))
    (unwind-protect
        (progn
          (with-temp-file tmp (insert "key: value\n"))
          (should-error (anvil-data-path-get tmp "key")))
      (delete-file tmp))))


;;;; --- value decoding -----------------------------------------------------

(ert-deftest anvil-data-test-decode-value-json-handles-scalars ()
  (should (= 42 (anvil-data--decode-value-json "42")))
  (should (equal "x" (anvil-data--decode-value-json "\"x\"")))
  (should (eq :null (anvil-data--decode-value-json "null")))
  (should (eq :false (anvil-data--decode-value-json "false"))))

(ert-deftest anvil-data-test-decode-value-json-handles-collections ()
  (should (equal [1 2 3] (anvil-data--decode-value-json "[1,2,3]")))
  (should (equal '(:k "v") (anvil-data--decode-value-json "{\"k\":\"v\"}"))))


(provide 'anvil-data-test)

;;; anvil-data-test.el ends here
