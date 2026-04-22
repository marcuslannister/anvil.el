;;; anvil-session-test.el --- ERT for anvil-session  -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

(let* ((this (or load-file-name buffer-file-name))
       (root (file-name-directory
              (directory-file-name (file-name-directory this)))))
  (add-to-list 'load-path root))

(require 'anvil-state)
(require 'anvil-session)

;;;; --- fixture helpers ----------------------------------------------------

(defmacro anvil-session-test--with-clean-state (&rest body)
  "Run BODY with a fresh anvil-state DB + isolated session namespaces.
Uses a temp DB path so parallel test runs don't trample each other's
snapshots, and purges both session namespaces before/after BODY."
  (declare (indent 0))
  `(let ((tmp (make-temp-file "anvil-session-test-" nil ".db")))
     (unwind-protect
         (let ((anvil-state-db-path tmp))
           ;; Force a fresh open on the isolated file.
           (when (fboundp 'anvil-state--close)
             (ignore-errors (anvil-state--close)))
           (ignore-errors (anvil-state-enable))
           (ignore-errors (anvil-state-delete-ns anvil-session--snapshot-ns))
           (ignore-errors (anvil-state-delete-ns anvil-session--events-ns))
           ,@body)
       (ignore-errors (anvil-state-delete-ns anvil-session--snapshot-ns))
       (ignore-errors (anvil-state-delete-ns anvil-session--events-ns))
       (when (fboundp 'anvil-state--close)
         (ignore-errors (anvil-state--close)))
       (ignore-errors (delete-file tmp)))))


;;;; --- Phase 1 primitives -------------------------------------------------

(ert-deftest anvil-session-test-snapshot-roundtrips ()
  "`session-snapshot' writes a plist that `session-resume' recovers
verbatim.  Locks the stored payload shape — name, created-at,
branch, base-branch, task-summary, notes, preamble-suggested all
survive the round trip."
  (anvil-session-test--with-clean-state
    (let* ((snap (anvil-session-snapshot
                  "demo"
                  :branch "feat/x"
                  :base-branch "develop"
                  :task-summary "Refactor worker"
                  :notes '("ran tests" "hit retry cap")))
           (got (anvil-session-resume "demo")))
      (should (equal (plist-get got :name) "demo"))
      (should (equal (plist-get got :branch) "feat/x"))
      (should (equal (plist-get got :base-branch) "develop"))
      (should (equal (plist-get got :task-summary) "Refactor worker"))
      (should (equal (plist-get got :notes)
                     '("ran tests" "hit retry cap")))
      (should (numberp (plist-get got :created-at)))
      (should (stringp (plist-get got :preamble-suggested)))
      (should (equal (plist-get got :name) (plist-get snap :name))))))

(ert-deftest anvil-session-test-preamble-contains-key-fields ()
  "`:preamble-suggested' carries name, branch / base-branch, and task
summary so a resuming Claude has a self-contained re-entry block
rather than a bag of plist keys."
  (anvil-session-test--with-clean-state
    (let* ((snap (anvil-session-snapshot
                  "refactor-anvil-worker"
                  :branch "feat/worker-v2"
                  :base-branch "develop"
                  :task-summary "Phase 4a backoff implementation"
                  :notes '("tests 508/510 pass")))
           (p (plist-get snap :preamble-suggested)))
      (should (string-match-p "refactor-anvil-worker" p))
      (should (string-match-p "feat/worker-v2" p))
      (should (string-match-p "develop" p))
      (should (string-match-p "Phase 4a backoff" p))
      (should (string-match-p "tests 508/510 pass" p)))))

(ert-deftest anvil-session-test-list-returns-descriptors ()
  "`session-list' returns one descriptor per live snapshot with the
locked field set (name / created-at / branch / task-summary-head),
truncating the task summary head to ≤80 chars."
  (anvil-session-test--with-clean-state
    (anvil-session-snapshot "one"
                            :branch "b1"
                            :task-summary "short summary")
    (anvil-session-snapshot "two"
                            :branch "b2"
                            :task-summary (make-string 200 ?x))
    (let* ((descs (anvil-session-list))
           (names (mapcar (lambda (d) (plist-get d :name)) descs)))
      (should (= (length descs) 2))
      (should (member "one" names))
      (should (member "two" names))
      (dolist (d descs)
        (let ((head (plist-get d :task-summary-head)))
          (should (stringp head))
          (should (<= (length head) 80)))))))

(ert-deftest anvil-session-test-delete-purges-row ()
  "`session-delete' returns t when a live snapshot was removed;
`session-resume' then returns nil and the descriptor drops from
`session-list'."
  (anvil-session-test--with-clean-state
    (anvil-session-snapshot "tmp" :task-summary "short-lived")
    (should (anvil-session-resume "tmp"))
    (should (anvil-session-delete "tmp"))
    (should-not (anvil-session-resume "tmp"))
    (should-not (cl-find-if
                 (lambda (d) (equal (plist-get d :name) "tmp"))
                 (anvil-session-list)))))

(ert-deftest anvil-session-test-missing-name-signals ()
  "`session-snapshot' with empty / nil NAME raises `user-error'
rather than silently writing to a meaningless key.  `session-delete'
follows the same contract.  `session-resume' of an unknown name
returns nil (non-existence is not an error)."
  (anvil-session-test--with-clean-state
    (should-error (anvil-session-snapshot ""))
    (should-error (anvil-session-snapshot nil))
    (should-error (anvil-session-delete ""))
    (should-not (anvil-session-resume "never-created"))))


(provide 'anvil-session-test)
;;; anvil-session-test.el ends here
