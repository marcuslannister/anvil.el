;;; anvil-extend-phaseD-test.el --- ERT for Phase D rationale auto-record (Doc 38 §3.D) -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT coverage for the Phase D rationale auto-record + secret
;; scanner surface added on top of the Phase A scaffold generator and
;; the Phase B hot-reload + filenotify watcher.  Each test rebinds
;; `anvil-extend-rationale-storage-dir' (and, where the scaffold
;; auto-hook is exercised, also `anvil-extend-storage-dir') to fresh
;; temp directories so the suite is hermetic and parallel-safe.
;;
;; The secret-scanner tests intentionally craft strings that match
;; the catalogue regexps (`sk-…', `ghp_…', `OPENAI_API_KEY=…',
;; PEM block markers) so the production redactor runs against real
;; payloads, not stubs.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'anvil-extend)

;; Phase D keeps its fixtures self-contained.  Loading this file does
;; not transitively re-load the Phase A or Phase B suites.

;;;; --- fixtures -----------------------------------------------------------

(defmacro anvil-extend-phaseD-test--with-rationale-dir (var &rest body)
  "Bind VAR to a fresh storage dir and `let' rationale storage to it.

The directory is removed at the end of BODY regardless of how it
exits.  When BODY does not reference VAR a trailing `(ignore VAR)'
suppresses the unused-lexical warning."
  (declare (indent 1))
  `(let* ((,var (file-name-as-directory
                 (make-temp-file "anvil-extend-phaseD-rationale-" t)))
          (anvil-extend-rationale-storage-dir ,var))
     (unwind-protect
         (progn (ignore ,var) ,@body)
       (when (file-directory-p ,var)
         (delete-directory ,var t)))))

(defmacro anvil-extend-phaseD-test--with-storage (var &rest body)
  "Bind VAR to a fresh storage dir and `let' `anvil-extend-storage-dir'."
  (declare (indent 1))
  `(let* ((,var (file-name-as-directory
                 (make-temp-file "anvil-extend-phaseD-storage-" t)))
          (anvil-extend-storage-dir ,var))
     (unwind-protect
         (progn (ignore ,var) ,@body)
       (when (file-directory-p ,var)
         (delete-directory ,var t)))))

(defmacro anvil-extend-phaseD-test--with-fixture (rationale storage &rest body)
  "Compose the rationale-dir fixture with the storage-dir fixture."
  (declare (indent 2))
  `(anvil-extend-phaseD-test--with-rationale-dir ,rationale
     (anvil-extend-phaseD-test--with-storage ,storage
       ,@body)))

(defun anvil-extend-phaseD-test--cleanup (sym)
  "Best-effort `unload-feature' for SYM and its `*-test' sibling."
  (dolist (s (list sym (intern (concat (symbol-name sym) "-test"))))
    (when (featurep s)
      (ignore-errors (unload-feature s t)))))

;;;; --- record-rationale ---------------------------------------------------

(ert-deftest anvil-extend-phaseD-test-record-creates-file ()
  "A successful record writes `extend_<NAME>.md' under storage."
  (anvil-extend-phaseD-test--with-rationale-dir dir
    (let ((res (anvil-extend-record-rationale 'pdf-page-count
                                              :purpose "count pages"
                                              :use-case "PDF audit"
                                              :tags '(pdf audit))))
      (should (eq :recorded (plist-get res :status)))
      (should (eq 'pdf-page-count (plist-get res :name)))
      (should (= 1 (plist-get res :version)))
      (should (file-exists-p (plist-get res :file)))
      (should (string-prefix-p dir (plist-get res :file)))
      (should (string-suffix-p "extend_pdf-page-count.md"
                               (plist-get res :file))))))

(ert-deftest anvil-extend-phaseD-test-record-rejects-bad-name ()
  "Record validates NAME via the Phase A regexp."
  (anvil-extend-phaseD-test--with-rationale-dir dir
    (should-error (anvil-extend-record-rationale "string-name"
                                                 :purpose "x")
                  :type 'user-error)
    (should-error (anvil-extend-record-rationale 'BadName
                                                 :purpose "x")
                  :type 'user-error)))

(ert-deftest anvil-extend-phaseD-test-record-bumps-version ()
  "Re-recording the same NAME increments :version, never overwrites."
  (anvil-extend-phaseD-test--with-rationale-dir dir
    (let* ((r1 (anvil-extend-record-rationale 'bump
                                              :purpose "v1 purpose"))
           (r2 (anvil-extend-record-rationale 'bump
                                              :purpose "v2 purpose"
                                              :change-summary "rev"))
           (r3 (anvil-extend-record-rationale 'bump
                                              :purpose "v3 purpose")))
      (should (= 1 (plist-get r1 :version)))
      (should (= 2 (plist-get r2 :version)))
      (should (= 3 (plist-get r3 :version)))
      ;; v1 + v2 headers are still in the file (history preserved).
      (with-temp-buffer
        (insert-file-contents (plist-get r3 :file))
        (let ((body (buffer-string)))
          (should (string-match-p "extend-bump rationale (v1)" body))
          (should (string-match-p "extend-bump rationale (v2)" body))
          (should (string-match-p "extend-bump rationale (v3)" body))
          (should (string-match-p "v1 purpose" body))
          (should (string-match-p "v2 purpose" body))
          (should (string-match-p "v3 purpose" body)))))))

;;;; --- recall-rationale ---------------------------------------------------

(ert-deftest anvil-extend-phaseD-test-recall-roundtrip ()
  "Record then recall returns the latest version's fields."
  (anvil-extend-phaseD-test--with-rationale-dir dir
    (anvil-extend-record-rationale 'roundtrip
                                   :purpose "do thing"
                                   :use-case "when X"
                                   :author "claude-session-A"
                                   :registry-kind 'permanent
                                   :policy 'shell-allowed
                                   :related-tools '(file-read git-status))
    (let ((rec (anvil-extend-recall-rationale 'roundtrip)))
      (should rec)
      (should (eq 'roundtrip (plist-get rec :name)))
      (should (= 1 (plist-get rec :version)))
      (should (equal "do thing" (plist-get rec :purpose)))
      (should (equal "when X"   (plist-get rec :use-case)))
      (should (equal "claude-session-A" (plist-get rec :author)))
      (should (eq 'permanent (plist-get rec :registry-kind)))
      (should (equal "shell-allowed" (plist-get rec :policy)))
      (should (equal '("file-read" "git-status")
                     (plist-get rec :related-tools))))))

(ert-deftest anvil-extend-phaseD-test-recall-missing-returns-nil ()
  "Recall returns nil when no rationale was ever recorded."
  (anvil-extend-phaseD-test--with-rationale-dir dir
    (should (null (anvil-extend-recall-rationale 'never-was)))))

(ert-deftest anvil-extend-phaseD-test-recall-after-bump-returns-latest ()
  "Recall after several recordings surfaces only the latest version."
  (anvil-extend-phaseD-test--with-rationale-dir dir
    (anvil-extend-record-rationale 'multi :purpose "first")
    (anvil-extend-record-rationale 'multi :purpose "second")
    (anvil-extend-record-rationale 'multi :purpose "third")
    (let ((rec (anvil-extend-recall-rationale 'multi)))
      (should (= 3 (plist-get rec :version)))
      (should (equal "third" (plist-get rec :purpose))))))

;;;; --- list-rationales ----------------------------------------------------

(ert-deftest anvil-extend-phaseD-test-list-empty ()
  "`anvil-extend-list-rationales' returns nil on an absent dir."
  (let ((anvil-extend-rationale-storage-dir
         (expand-file-name
          (format "anvil-extend-phaseD-empty-%d/" (random 99999))
          temporary-file-directory)))
    (unwind-protect
        (should (null (anvil-extend-list-rationales)))
      (when (file-directory-p anvil-extend-rationale-storage-dir)
        (delete-directory anvil-extend-rationale-storage-dir t)))))

(ert-deftest anvil-extend-phaseD-test-list-aggregates ()
  "Listing returns one entry per rationale file, sorted by NAME."
  (anvil-extend-phaseD-test--with-rationale-dir dir
    (anvil-extend-record-rationale 'beta-tool :purpose "B")
    (anvil-extend-record-rationale 'alpha-tool :purpose "A")
    (anvil-extend-record-rationale 'gamma-tool :purpose "G")
    (let ((rows (anvil-extend-list-rationales)))
      (should (= 3 (length rows)))
      (should (equal '(alpha-tool beta-tool gamma-tool)
                     (mapcar (lambda (r) (plist-get r :name)) rows)))
      (should (equal '("A" "B" "G")
                     (mapcar (lambda (r) (plist-get r :purpose-summary))
                             rows))))))

;;;; --- secret scanner -----------------------------------------------------

(ert-deftest anvil-extend-phaseD-test-scan-secrets-detects-sk-key ()
  "Scanner flags an OpenAI-style `sk-…' key with at least 16 trailing chars."
  (let ((findings (anvil-extend--scan-secrets
                   "Token is sk-ABCdef0123456789mNoP and that's it.")))
    (should findings)
    (should (cl-some (lambda (f)
                       (string-prefix-p "sk-" (plist-get f :match)))
                     findings))))

(ert-deftest anvil-extend-phaseD-test-redact-replaces-secret ()
  "Redact replaces the secret payload with `[REDACTED]' marker."
  (let* ((text  "Use sk-ABCdef0123456789mNoP for auth")
         (pair  (anvil-extend--redact-secrets text))
         (out   (car pair))
         (count (length (cdr pair))))
    (should (>= count 1))
    (should-not (string-match-p "sk-ABCdef0123456789mNoP" out))
    (should (string-match-p "sk-\\[REDACTED\\]" out))))

(ert-deftest anvil-extend-phaseD-test-record-redacts-secret-in-purpose ()
  "Recording a rationale with a secret-laden purpose stores it redacted."
  (anvil-extend-phaseD-test--with-rationale-dir dir
    (let* ((res (anvil-extend-record-rationale 'leaky
                                               :purpose "key=ghp_AbCdEfGhIjKlMnOpQrStUv now"))
           (rec (anvil-extend-recall-rationale 'leaky)))
      (should (>= (plist-get res :redactions) 1))
      (should-not (string-match-p "ghp_AbCdEfGhIjKlMnOpQrStUv"
                                  (plist-get rec :purpose)))
      (should (string-match-p "ghp_\\[REDACTED\\]"
                              (plist-get rec :purpose))))))

(ert-deftest anvil-extend-phaseD-test-allow-pattern-suppresses-redaction ()
  "A whitelisted line is not redacted even if a regex matches."
  (let* ((anvil-extend-secret-scan-allow-patterns '("FIXTURE_KEY"))
         (text "FIXTURE_KEY=sk-ABCdef0123456789mNoP example")
         (pair (anvil-extend--redact-secrets text)))
    ;; The whitelist suppresses both the env-var pattern AND the
    ;; `sk-…' pattern because the same line carries both matches.
    (should (zerop (length (cdr pair))))
    (should (string-match-p "sk-ABCdef0123456789mNoP" (car pair)))))

;;;; --- scaffold auto-hook -------------------------------------------------

(ert-deftest anvil-extend-phaseD-test-scaffold-auto-records-rationale ()
  "Scaffold + :purpose auto-records a v1 rationale entry."
  (anvil-extend-phaseD-test--with-fixture rationale storage
    (unwind-protect
        (let* ((res (anvil-extend-scaffold 'autohook
                                           :params '(x)
                                           :body '((+ x 1))
                                           :docstring "auto-hook fixture"
                                           :purpose "scaffold auto-hook"
                                           :use-case "phase D smoke"
                                           :tags '(test phaseD))))
          (should (eq :created (plist-get res :status)))
          (let ((r (plist-get res :rationale)))
            (should r)
            (should (eq :recorded (plist-get r :status)))
            (should (eq 'autohook (plist-get r :name)))
            (should (= 1 (plist-get r :version))))
          (let ((rec (anvil-extend-recall-rationale 'autohook)))
            (should rec)
            (should (equal "scaffold auto-hook"
                           (plist-get rec :purpose)))))
      (anvil-extend-phaseD-test--cleanup 'autohook))))

(ert-deftest anvil-extend-phaseD-test-scaffold-without-purpose-skips-record ()
  "Scaffold without :purpose leaves :rationale nil and no file written."
  (anvil-extend-phaseD-test--with-fixture rationale storage
    (unwind-protect
        (let ((res (anvil-extend-scaffold 'no-rationale
                                          :body '(nil))))
          (should (eq :created (plist-get res :status)))
          (should (null (plist-get res :rationale)))
          (should (null (anvil-extend-recall-rationale 'no-rationale))))
      (anvil-extend-phaseD-test--cleanup 'no-rationale))))

;;;; --- MCP wrappers -------------------------------------------------------

(ert-deftest anvil-extend-phaseD-test-mcp-record-accepts-string-name ()
  "MCP wrapper coerces a string NAME to a symbol before recording."
  (anvil-extend-phaseD-test--with-rationale-dir dir
    (cl-letf (((symbol-function 'anvil-server-with-error-handling)
               (lambda (&rest body)
                 (eval (cons 'progn body) t))))
      (let* ((str (anvil-extend--tool-record-rationale
                   "mcp-string"
                   "via mcp" "use case" nil "alpha,beta" nil nil nil))
             (parsed (read str)))
        (should (eq :recorded (plist-get parsed :status)))
        (should (eq 'mcp-string (plist-get parsed :name)))
        (let ((rec (anvil-extend-recall-rationale 'mcp-string)))
          (should rec)
          (should (equal "via mcp" (plist-get rec :purpose)))
          (should (equal '("alpha" "beta") (plist-get rec :tags))))))))

(ert-deftest anvil-extend-phaseD-test-mcp-list-returns-printed-plist ()
  "List MCP wrapper returns a printed Elisp object readable via `read'."
  (anvil-extend-phaseD-test--with-rationale-dir dir
    (cl-letf (((symbol-function 'anvil-server-with-error-handling)
               (lambda (&rest body)
                 (eval (cons 'progn body) t))))
      (anvil-extend-record-rationale 'list-mcp :purpose "p")
      (let* ((str (anvil-extend--tool-list-rationales))
             (parsed (read str)))
        (should (listp parsed))
        (should (= 1 (length parsed)))
        (should (eq 'list-mcp (plist-get (car parsed) :name)))))))

(provide 'anvil-extend-phaseD-test)
;;; anvil-extend-phaseD-test.el ends here
