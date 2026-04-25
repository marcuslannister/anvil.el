;;; anvil-extend-phaseF-test.el --- ERT for Phase F promotion gate (Doc 38 §3.F) -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT coverage for the Phase F promotion / demotion / pool-query
;; surface and the pre-execution review advice added on top of
;; Phases A / B / C / D.  Each test rebinds both
;; `anvil-extend-storage-dir' (= ephemeral pool) and
;; `anvil-extend-permanent-storage-dir' (= permanent pool) plus
;; `anvil-extend-rationale-storage-dir' to fresh temp directories so
;; the suite is hermetic and parallel-safe.
;;
;; Tests intentionally exercise the production move/copy code paths
;; against real on-disk files (no stubbed fs) so the rollback /
;; cleanup logic runs against realistic scaffolds; the smoke ERT
;; helper from Phase D's fixture is reused so a generated extension
;; passes its own scaffolded test before promotion.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'anvil-extend)

;; Phase F keeps its fixtures self-contained so loading this file does
;; not transitively re-run any other phase's suite.

;;;; --- fixtures -----------------------------------------------------------

(defmacro anvil-extend-phaseF-test--with-pools
    (eph perm rationale &rest body)
  "Bind EPH / PERM / RATIONALE to fresh temp dirs and rebind storage.

Resets `anvil-extend--promotion-policy' to the deny-all-but-tests
defcustom default before BODY so prior tests can flip a key
without cross-test pollution.  All three directories are removed
at the end of BODY regardless of how it exits."
  (declare (indent 3))
  `(let* ((,eph (file-name-as-directory
                 (make-temp-file "anvil-extend-phaseF-eph-" t)))
          (,perm (file-name-as-directory
                  (make-temp-file "anvil-extend-phaseF-perm-" t)))
          (,rationale (file-name-as-directory
                       (make-temp-file "anvil-extend-phaseF-rat-" t)))
          (anvil-extend-storage-dir ,eph)
          (anvil-extend-permanent-storage-dir ,perm)
          (anvil-extend-rationale-storage-dir ,rationale)
          (anvil-extend--promotion-policy
           (copy-sequence anvil-extend-promotion-default-policy))
          (anvil-extend--pre-execution-review-log nil))
     (unwind-protect
         (progn (ignore ,eph ,perm ,rationale) ,@body)
       (dolist (d (list ,eph ,perm ,rationale))
         (when (file-directory-p d)
           (delete-directory d t))))))

(defun anvil-extend-phaseF-test--scaffold (name &optional purpose)
  "Helper: scaffold NAME with a trivial body and optional PURPOSE."
  (anvil-extend-scaffold
   name
   :params nil
   :body '((+ 1 1))
   :docstring "phaseF fixture body"
   :purpose purpose
   :tags '(phaseF fixture)))

(defun anvil-extend-phaseF-test--cleanup (sym)
  "Best-effort `unload-feature' for SYM and its `*-test' sibling."
  (dolist (s (list sym (intern (concat (symbol-name sym) "-test"))))
    (when (featurep s)
      (ignore-errors (unload-feature s t)))))

;;;; --- pool-status --------------------------------------------------------

(ert-deftest anvil-extend-phaseF-test-pool-status-nil-when-absent ()
  "Pool status returns nil when NAME has no scaffold in either pool."
  (anvil-extend-phaseF-test--with-pools eph perm rat
    (should (null (anvil-extend-pool-status 'never-scaffolded)))))

(ert-deftest anvil-extend-phaseF-test-pool-status-ephemeral ()
  "Pool status returns `ephemeral' after scaffold."
  (anvil-extend-phaseF-test--with-pools eph perm rat
    (anvil-extend-phaseF-test--scaffold 'eph-only "purpose")
    (unwind-protect
        (should (eq 'ephemeral
                    (anvil-extend-pool-status 'eph-only)))
      (anvil-extend-phaseF-test--cleanup 'eph-only))))

(ert-deftest anvil-extend-phaseF-test-pool-status-permanent-wins ()
  "When NAME exists in both pools, status reports `permanent'."
  (anvil-extend-phaseF-test--with-pools eph perm rat
    (anvil-extend-phaseF-test--scaffold 'both-pools "purpose")
    (unwind-protect
        (let* ((paths (anvil-extend--permanent-paths 'both-pools))
               (dest (plist-get paths :elisp-file)))
          (anvil-extend--ensure-permanent-storage)
          (copy-file (plist-get (anvil-extend--paths 'both-pools)
                                :elisp-file)
                     dest t t)
          (should (eq 'permanent
                      (anvil-extend-pool-status 'both-pools))))
      (anvil-extend-phaseF-test--cleanup 'both-pools))))

;;;; --- list-by-pool -------------------------------------------------------

(ert-deftest anvil-extend-phaseF-test-list-by-pool-filters ()
  "`list-by-pool' returns only the requested pool."
  (anvil-extend-phaseF-test--with-pools eph perm rat
    (anvil-extend-phaseF-test--scaffold 'eph-tool "purpose-eph")
    (anvil-extend-phaseF-test--scaffold 'perm-tool "purpose-perm")
    (unwind-protect
        (progn
          ;; Manually move perm-tool into the permanent pool.
          (let* ((src (plist-get (anvil-extend--paths 'perm-tool)
                                 :elisp-file))
                 (dst (plist-get (anvil-extend--permanent-paths 'perm-tool)
                                 :elisp-file)))
            (anvil-extend--ensure-permanent-storage)
            (rename-file src dst t))
          (let ((eph-list (anvil-extend-list-by-pool 'ephemeral))
                (perm-list (anvil-extend-list-by-pool 'permanent))
                (all-list (anvil-extend-list-by-pool nil)))
            (should (= 1 (length eph-list)))
            (should (= 1 (length perm-list)))
            (should (= 2 (length all-list)))
            (should (eq 'eph-tool (plist-get (car eph-list) :name)))
            (should (eq 'ephemeral (plist-get (car eph-list) :pool)))
            (should (eq 'perm-tool (plist-get (car perm-list) :name)))
            (should (eq 'permanent (plist-get (car perm-list) :pool)))))
      (anvil-extend-phaseF-test--cleanup 'eph-tool)
      (anvil-extend-phaseF-test--cleanup 'perm-tool))))

(ert-deftest anvil-extend-phaseF-test-list-by-pool-includes-rationale ()
  "Each row carries `:rationale' (recall plist or nil)."
  (anvil-extend-phaseF-test--with-pools eph perm rat
    (anvil-extend-phaseF-test--scaffold 'with-purpose "doc thing")
    (anvil-extend-phaseF-test--scaffold 'without-purpose nil)
    (unwind-protect
        (let ((rows (anvil-extend-list-by-pool 'ephemeral)))
          (should (= 2 (length rows)))
          (let ((row-with (cl-find 'with-purpose rows
                                   :key (lambda (r) (plist-get r :name))))
                (row-without (cl-find 'without-purpose rows
                                      :key (lambda (r) (plist-get r :name)))))
            (should row-with)
            (should row-without)
            (should (plist-get row-with :rationale))
            (should (null (plist-get row-without :rationale)))
            (should (equal "doc thing"
                           (plist-get (plist-get row-with :rationale)
                                      :purpose)))))
      (anvil-extend-phaseF-test--cleanup 'with-purpose)
      (anvil-extend-phaseF-test--cleanup 'without-purpose))))

;;;; --- promote: happy path ------------------------------------------------

(ert-deftest anvil-extend-phaseF-test-promote-moves-to-permanent ()
  "Promote moves files to permanent pool; pool-status flips."
  (anvil-extend-phaseF-test--with-pools eph perm rat
    (anvil-extend-phaseF-test--scaffold 'pdf-page-count "count pages")
    (unwind-protect
        (let ((res (anvil-extend-promote 'pdf-page-count
                                         :signoff 'user)))
          (should (eq :promoted (plist-get res :status)))
          (should (eq 'pdf-page-count (plist-get res :name)))
          (should (eq 'permanent (plist-get res :pool)))
          (should (file-exists-p (plist-get res :path)))
          ;; Destination triplet present.
          (let ((dest (anvil-extend--permanent-paths 'pdf-page-count)))
            (should (file-exists-p (plist-get dest :elisp-file)))
            (should (file-exists-p (plist-get dest :test-file)))
            (should (file-exists-p (plist-get dest :register-stub))))
          ;; Ephemeral originals deleted.
          (let ((src (anvil-extend--paths 'pdf-page-count)))
            (should-not (file-exists-p (plist-get src :elisp-file)))
            (should-not (file-exists-p (plist-get src :test-file))))
          (should (eq 'permanent
                      (anvil-extend-pool-status 'pdf-page-count))))
      (anvil-extend-phaseF-test--cleanup 'pdf-page-count))))

(ert-deftest anvil-extend-phaseF-test-promote-records-change-summary ()
  "Promote bumps the rationale version with `:change-summary'."
  (anvil-extend-phaseF-test--with-pools eph perm rat
    (anvil-extend-phaseF-test--scaffold 'audit-tool "audit thing")
    (unwind-protect
        (let* ((res (anvil-extend-promote 'audit-tool
                                          :signoff 'user
                                          :change-summary "ship to prod"))
               (rec (anvil-extend-recall-rationale 'audit-tool)))
          (should (eq :promoted (plist-get res :status)))
          (should rec)
          ;; v1 was the scaffold record; promotion bumps to v2.
          (should (>= (plist-get rec :version) 2))
          (should (eq 'permanent (plist-get rec :registry-kind))))
      (anvil-extend-phaseF-test--cleanup 'audit-tool))))

;;;; --- promote: policy enforcement ---------------------------------------

(ert-deftest anvil-extend-phaseF-test-promote-rejects-without-signoff ()
  "Default policy `:require-signoff t' signals on bare promote."
  (anvil-extend-phaseF-test--with-pools eph perm rat
    (anvil-extend-phaseF-test--scaffold 'no-signoff "purpose")
    (unwind-protect
        (should-error
         (anvil-extend-promote 'no-signoff)
         :type 'anvil-extend-promotion-policy-violation)
      (anvil-extend-phaseF-test--cleanup 'no-signoff))))

(ert-deftest anvil-extend-phaseF-test-promote-rejects-without-rationale ()
  "Default policy `:require-rationale t' rejects names without record."
  (anvil-extend-phaseF-test--with-pools eph perm rat
    ;; Scaffold without :purpose so no rationale is auto-recorded.
    (anvil-extend-scaffold 'no-rationale-tool
                           :body '(nil)
                           :docstring "no rationale")
    (unwind-protect
        (should-error
         (anvil-extend-promote 'no-rationale-tool :signoff 'user)
         :type 'anvil-extend-promotion-policy-violation)
      (anvil-extend-phaseF-test--cleanup 'no-rationale-tool))))

(ert-deftest anvil-extend-phaseF-test-promote-rejects-missing-source ()
  "Promoting a never-scaffolded NAME returns `:rejected :missing-source'."
  (anvil-extend-phaseF-test--with-pools eph perm rat
    (let ((res (anvil-extend-promote 'never-scaffolded :signoff 'user)))
      (should (eq :rejected (plist-get res :status)))
      (should (eq :missing-source (plist-get res :reason))))))

(ert-deftest anvil-extend-phaseF-test-promote-policy-relaxation ()
  "Flipping `:require-rationale nil' lets a no-purpose scaffold through."
  (anvil-extend-phaseF-test--with-pools eph perm rat
    (anvil-extend-scaffold 'relax-tool
                           :body '(nil)
                           :docstring "no rationale")
    (unwind-protect
        (progn
          (anvil-extend-promotion-policy :require-rationale nil)
          (let ((res (anvil-extend-promote 'relax-tool :signoff 'user)))
            (should (eq :promoted (plist-get res :status)))))
      (anvil-extend-phaseF-test--cleanup 'relax-tool))))

;;;; --- demote -------------------------------------------------------------

(ert-deftest anvil-extend-phaseF-test-demote-moves-back ()
  "Demote inverts promote: file returns to ephemeral pool."
  (anvil-extend-phaseF-test--with-pools eph perm rat
    (anvil-extend-phaseF-test--scaffold 'roundtrip-tool "roundtrip")
    (unwind-protect
        (progn
          (anvil-extend-promote 'roundtrip-tool :signoff 'user)
          (should (eq 'permanent
                      (anvil-extend-pool-status 'roundtrip-tool)))
          (let ((res (anvil-extend-demote 'roundtrip-tool)))
            (should (eq :demoted (plist-get res :status)))
            (should (eq 'ephemeral (plist-get res :pool)))
            (should (file-exists-p (plist-get res :path))))
          (should (eq 'ephemeral
                      (anvil-extend-pool-status 'roundtrip-tool)))
          (let ((perm-paths (anvil-extend--permanent-paths
                             'roundtrip-tool)))
            (should-not
             (file-exists-p (plist-get perm-paths :elisp-file)))))
      (anvil-extend-phaseF-test--cleanup 'roundtrip-tool))))

(ert-deftest anvil-extend-phaseF-test-demote-rejects-missing-source ()
  "Demoting an absent permanent NAME returns `:rejected :missing-source'."
  (anvil-extend-phaseF-test--with-pools eph perm rat
    (let ((res (anvil-extend-demote 'never-promoted)))
      (should (eq :rejected (plist-get res :status)))
      (should (eq :missing-source (plist-get res :reason))))))

;;;; --- promotion-policy ---------------------------------------------------

(ert-deftest anvil-extend-phaseF-test-promotion-policy-update ()
  "`promotion-policy' merges keys without clobbering siblings."
  (anvil-extend-phaseF-test--with-pools eph perm rat
    (let ((before (copy-sequence anvil-extend--promotion-policy)))
      (anvil-extend-promotion-policy :require-tests t)
      (should (plist-get anvil-extend--promotion-policy :require-tests))
      ;; Other keys preserved.
      (should (eq (plist-get before :require-signoff)
                  (plist-get anvil-extend--promotion-policy
                             :require-signoff)))
      (should (eq (plist-get before :require-rationale)
                  (plist-get anvil-extend--promotion-policy
                             :require-rationale))))))

(ert-deftest anvil-extend-phaseF-test-promotion-policy-reset ()
  "Reset restores the defcustom default plist, no shared structure."
  (anvil-extend-phaseF-test--with-pools eph perm rat
    (anvil-extend-promotion-policy :require-tests t
                                   :pre-execution-review t)
    (anvil-extend-promotion-policy-reset)
    (should-not (plist-get anvil-extend--promotion-policy
                           :require-tests))
    (should-not (plist-get anvil-extend--promotion-policy
                           :pre-execution-review))
    ;; Mutating the live policy must not leak into the defcustom
    ;; default (= no shared structure).
    (anvil-extend-promotion-policy :require-tests t)
    (should-not (plist-get anvil-extend-promotion-default-policy
                           :require-tests))))

;;;; --- pre-execution review advice ---------------------------------------

(ert-deftest anvil-extend-phaseF-test-advice-noop-when-policy-off ()
  "With `:pre-execution-review nil' (default) the advice records nothing."
  (anvil-extend-phaseF-test--with-pools eph perm rat
    ;; Drive the advice via the recorder helper directly so we
    ;; don't depend on anvil-offload availability for this test.
    (anvil-extend--pre-execution-review-advice '(+ 1 1))
    (should (null anvil-extend--pre-execution-review-log))))

(ert-deftest anvil-extend-phaseF-test-advice-records-when-policy-on ()
  "Flipping `:pre-execution-review t' records every reviewed form."
  (anvil-extend-phaseF-test--with-pools eph perm rat
    (anvil-extend-promotion-policy :pre-execution-review t)
    (anvil-extend--pre-execution-review-advice '(+ 1 2))
    (anvil-extend--pre-execution-review-advice '(message "hi"))
    (should (= 2 (length anvil-extend--pre-execution-review-log)))
    (should (equal '(+ 1 2)
                   (plist-get
                    (cadr anvil-extend--pre-execution-review-log)
                    :form)))))

(ert-deftest anvil-extend-phaseF-test-advice-installed-on-eval-sandboxed ()
  "Advice is installed by module load, not just by enable."
  ;; The advice is added at top level when anvil-extend.el is loaded;
  ;; assert it shows up in the advice ring of the target function.
  (let ((found nil))
    (advice-mapc
     (lambda (fn _props)
       (when (eq fn #'anvil-extend--pre-execution-review-advice)
         (setq found t)))
     'anvil-extend-eval-sandboxed)
    (should found)))

;;;; --- MCP wrappers -------------------------------------------------------

(ert-deftest anvil-extend-phaseF-test-mcp-pool-status-string-name ()
  "`pool-status' MCP wrapper coerces a string NAME to symbol."
  (anvil-extend-phaseF-test--with-pools eph perm rat
    (anvil-extend-phaseF-test--scaffold 'mcp-tool "purpose")
    (unwind-protect
        (cl-letf (((symbol-function 'anvil-server-with-error-handling)
                   (lambda (&rest body)
                     (eval (cons 'progn body) t))))
          (let* ((str (anvil-extend--tool-pool-status "mcp-tool"))
                 (parsed (read str)))
            (should (eq 'ephemeral parsed))))
      (anvil-extend-phaseF-test--cleanup 'mcp-tool))))

(ert-deftest anvil-extend-phaseF-test-mcp-list-by-pool-string ()
  "`list-by-pool' MCP wrapper accepts a string POOL."
  (anvil-extend-phaseF-test--with-pools eph perm rat
    (anvil-extend-phaseF-test--scaffold 'mcp-list "purpose")
    (unwind-protect
        (cl-letf (((symbol-function 'anvil-server-with-error-handling)
                   (lambda (&rest body)
                     (eval (cons 'progn body) t))))
          (let* ((str (anvil-extend--tool-list-by-pool "ephemeral"))
                 (parsed (read str)))
            (should (listp parsed))
            (should (= 1 (length parsed)))
            (should (eq 'mcp-list (plist-get (car parsed) :name)))))
      (anvil-extend-phaseF-test--cleanup 'mcp-list))))

(ert-deftest anvil-extend-phaseF-test-mcp-promote-rejected-data ()
  "`promote' MCP wrapper translates policy violation into `:rejected'."
  (anvil-extend-phaseF-test--with-pools eph perm rat
    (anvil-extend-phaseF-test--scaffold 'mcp-promote "purpose")
    (unwind-protect
        (cl-letf (((symbol-function 'anvil-server-with-error-handling)
                   (lambda (&rest body)
                     (eval (cons 'progn body) t))))
          ;; signoff omitted -> policy violation -> wrapper returns
          ;; data, never raises.
          (let* ((str (anvil-extend--tool-promote "mcp-promote"))
                 (parsed (read str)))
            (should (eq :rejected (plist-get parsed :status)))
            (should (eq :policy-violation (plist-get parsed :reason)))))
      (anvil-extend-phaseF-test--cleanup 'mcp-promote))))

(provide 'anvil-extend-phaseF-test)
;;; anvil-extend-phaseF-test.el ends here
