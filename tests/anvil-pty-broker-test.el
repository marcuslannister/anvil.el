;;; anvil-pty-broker-test.el --- ERT for anvil-pty-broker -*- lexical-binding: t; -*-

(require 'ert)
(require 'json)

(let* ((this (or load-file-name buffer-file-name))
       (root (file-name-directory (directory-file-name (file-name-directory this)))))
  (add-to-list 'load-path root))

(require 'anvil-server)
(require 'anvil-pty-broker)

(defun anvil-pty-broker-test--live-available-p ()
  "Return non-nil when the Node broker and node-pty are installed."
  (and (executable-find anvil-pty-broker-node-binary)
       (file-exists-p anvil-pty-broker-script)
       (file-exists-p
        (expand-file-name "pty-broker/node_modules/node-pty/package.json"
                          (file-name-directory
                           (directory-file-name
                            (file-name-directory anvil-pty-broker-script)))))
       (not (getenv "ANVIL_SKIP_LIVE"))))

(defmacro anvil-pty-broker-test--with-clean-state (&rest body)
  "Run BODY with a fresh pty-broker state hash."
  (declare (indent 0))
  `(let ((anvil-pty-broker--ptys (make-hash-table :test 'equal))
         (anvil-pty-broker--recv-buf ""))
     ,@body))

;;; --- pure helpers (no network, always run) -----------------------------

(ert-deftest anvil-pty-broker-test/gen-token-shape ()
  (let ((t1 (anvil-pty-broker--gen-token))
        (t2 (anvil-pty-broker--gen-token)))
    (should (= 32 (length t1)))
    (should (= 32 (length t2)))
    (should (string-match-p "\\`[A-Za-z0-9]+\\'" t1))
    (should-not (equal t1 t2))))

(ert-deftest anvil-pty-broker-test/encode-frame-trailing-lf ()
  (let ((frame (anvil-pty-broker--encode-frame '(:op "auth" :token "abc"))))
    (should (string-suffix-p "\n" frame))
    (should (string-match-p "\"op\":\"auth\"" frame))
    (should (string-match-p "\"token\":\"abc\"" frame))))

(ert-deftest anvil-pty-broker-test/drain-tick-parses-frames ()
  (anvil-pty-broker-test--with-clean-state
    (setq anvil-pty-broker--recv-buf
          (concat (json-encode '(:ev "authed")) "\n"
                  (json-encode '(:ev "spawned" :id "p1" :pid 12345)) "\n"
                  (json-encode `(:ev "output" :id "p1"
                                 :data ,(base64-encode-string "hi" t)))
                  "\n"))
    (anvil-pty-broker--drain-tick)
    (should anvil-pty-broker--authed)
    (let ((row (gethash "p1" anvil-pty-broker--ptys)))
      (should (= 12345 (plist-get row :pid)))
      (should (equal "hi" (plist-get row :output))))
    (setq anvil-pty-broker--authed nil)))

(ert-deftest anvil-pty-broker-test/drain-tick-keeps-partial-line ()
  (anvil-pty-broker-test--with-clean-state
    (setq anvil-pty-broker--recv-buf "{\"ev\":\"auth")
    (anvil-pty-broker--drain-tick)
    (should (equal "{\"ev\":\"auth" anvil-pty-broker--recv-buf))
    (setq anvil-pty-broker--recv-buf
          (concat anvil-pty-broker--recv-buf "ed\"}\n"))
    (anvil-pty-broker--drain-tick)
    (should (string-empty-p anvil-pty-broker--recv-buf))
    (should anvil-pty-broker--authed)
    (setq anvil-pty-broker--authed nil)))

(ert-deftest anvil-pty-broker-test/drain-tick-records-error-event ()
  (anvil-pty-broker-test--with-clean-state
    (setq anvil-pty-broker--recv-buf
          (concat (json-encode '(:ev "error" :id "p7" :message "boom"))
                  "\n"))
    (anvil-pty-broker--drain-tick)
    (let ((row (gethash "p7" anvil-pty-broker--ptys)))
      (should row)
      (should (cl-some (lambda (e) (equal (plist-get e :message) "boom"))
                       (plist-get row :events))))))

(ert-deftest anvil-pty-broker-test/drain-tick-exit-records-code ()
  (anvil-pty-broker-test--with-clean-state
    (setq anvil-pty-broker--recv-buf
          (concat (json-encode '(:ev "exit" :id "q1" :code 0)) "\n"))
    (anvil-pty-broker--drain-tick)
    (let ((row (gethash "q1" anvil-pty-broker--ptys)))
      (should (= 0 (plist-get row :exit))))))

(ert-deftest anvil-pty-broker-test/pty-read-returns-output-and-optional-consume ()
  (anvil-pty-broker-test--with-clean-state
    (puthash "abc" (list :output "hello" :events nil) anvil-pty-broker--ptys)
    (should (equal "hello" (anvil-pty-read "abc")))
    ;; non-consuming read keeps the buffer
    (should (equal "hello" (anvil-pty-read "abc")))
    ;; consuming read drains it
    (should (equal "hello" (anvil-pty-read "abc" t)))
    (should (equal "" (anvil-pty-read "abc")))))

(ert-deftest anvil-pty-broker-test/tool-read-coerces-consume ()
  (anvil-pty-broker-test--with-clean-state
    (puthash "abc" (list :output "x" :events nil) anvil-pty-broker--ptys)
    (let ((r (anvil-pty-broker--tool-read "abc" "")))
      (should (equal "x" (plist-get r :output)))
      (should-not (plist-get r :consumed)))
    (let ((r (anvil-pty-broker--tool-read "abc" "t")))
      (should (plist-get r :consumed)))
    (should (equal "" (anvil-pty-read "abc")))))

(ert-deftest anvil-pty-broker-test/tool-spawn-requires-allowlist-indirect ()
  "The wrapper forwards to `anvil-pty-spawn'; when not authed it errors."
  (anvil-pty-broker-test--with-clean-state
    (let ((anvil-pty-broker--authed nil))
      (should-error (anvil-pty-broker--tool-spawn "bash")
                    :type 'error))))

;;; --- Phase 2b: pty-read-filtered (tail + shell-filter integration) ------

(defun anvil-pty-broker-test--shell-filter-loaded-p ()
  "Return non-nil when anvil-shell-filter is loaded in this process."
  (and (require 'anvil-shell-filter nil t)
       (fboundp 'anvil-shell-filter-apply)))

(ert-deftest anvil-pty-broker-test/read-filtered-applies-filter ()
  "pty-read-filtered routes raw output through a named shell-filter and
reports raw-size vs filtered-size.  When shell-filter is loaded the
compressed size must be strictly smaller than the raw for a filter
that's expected to help (docker-logs on a deduplicable sample)."
  (skip-unless (anvil-pty-broker-test--shell-filter-loaded-p))
  (anvil-pty-broker-test--with-clean-state
    (puthash "p1"
             (list :output
                   "2026-04-22T10:00:00Z app started
2026-04-22T10:00:01Z heartbeat
2026-04-22T10:00:02Z heartbeat
2026-04-22T10:00:03Z heartbeat
2026-04-22T10:00:04Z heartbeat
"
                   :events nil
                   :tail-cursor 0)
             anvil-pty-broker--ptys)
    (let ((r (anvil-pty-read-filtered "p1" 'docker-logs nil nil)))
      (should (equal (plist-get r :id) "p1"))
      (should (equal (plist-get r :filter) "docker-logs"))
      (should (integerp (plist-get r :raw-size)))
      (should (integerp (plist-get r :filtered-size)))
      (should (< (plist-get r :filtered-size)
                 (plist-get r :raw-size)))
      (should (stringp (plist-get r :output))))))

(ert-deftest anvil-pty-broker-test/read-filtered-tail-advances-cursor ()
  "With TAIL non-nil each call returns only output appended since the
previous tail read, advancing the per-pty tail-cursor.  Models the
streaming log-tail use case that Phase 2b was cut out for."
  (anvil-pty-broker-test--with-clean-state
    (puthash "p2"
             (list :output "chunk-1\n" :events nil :tail-cursor 0)
             anvil-pty-broker--ptys)
    (let ((r1 (anvil-pty-read-filtered "p2" nil t nil)))
      (should (equal (plist-get r1 :output) "chunk-1\n"))
      (should (= (plist-get r1 :tail-cursor) (length "chunk-1\n"))))
    ;; Simulate the broker appending a new chunk between reads.
    (let ((row (gethash "p2" anvil-pty-broker--ptys)))
      (plist-put row :output "chunk-1\nchunk-2\n"))
    (let ((r2 (anvil-pty-read-filtered "p2" nil t nil)))
      ;; Only the new slice is returned, cursor advanced to new end.
      (should (equal (plist-get r2 :output) "chunk-2\n"))
      (should (= (plist-get r2 :raw-size) (length "chunk-2\n")))
      (should (= (plist-get r2 :tail-cursor)
                 (length "chunk-1\nchunk-2\n"))))))

(ert-deftest anvil-pty-broker-test/read-filtered-nil-filter-passthrough ()
  "A nil / empty filter returns the raw text unchanged, with
filtered-size == raw-size.  Lets callers use tail-cursor bookkeeping
without requiring a shell-filter handler."
  (anvil-pty-broker-test--with-clean-state
    (puthash "p3"
             (list :output "abc" :events nil :tail-cursor 0)
             anvil-pty-broker--ptys)
    (let ((r (anvil-pty-read-filtered "p3" nil nil nil)))
      (should (equal (plist-get r :output) "abc"))
      (should (= (plist-get r :raw-size) 3))
      (should (= (plist-get r :filtered-size) 3))
      (should (null (plist-get r :filter))))
    (let ((r (anvil-pty-read-filtered "p3" "" nil nil)))
      (should (equal (plist-get r :output) "abc"))
      (should (null (plist-get r :filter))))))

(ert-deftest anvil-pty-broker-test/read-filtered-missing-pty-signals ()
  "Unknown pty ids raise — the tool is honest about not silently
returning empty strings for ids that never existed."
  (anvil-pty-broker-test--with-clean-state
    (should-error (anvil-pty-read-filtered "no-such" nil nil nil))))

(ert-deftest anvil-pty-broker-test/read-filtered-consume-resets-cursor ()
  "CONSUME=t drains :output and resets the tail cursor, matching
`anvil-pty-read' semantics but on the filtered variant."
  (anvil-pty-broker-test--with-clean-state
    (puthash "p4"
             (list :output "hello" :events nil :tail-cursor 3)
             anvil-pty-broker--ptys)
    (let ((r (anvil-pty-read-filtered "p4" nil nil t)))
      (should (equal (plist-get r :output) "hello"))
      (should (plist-get r :consumed)))
    (let ((row (gethash "p4" anvil-pty-broker--ptys)))
      (should (equal (plist-get row :output) ""))
      (should (= (plist-get row :tail-cursor) 0)))))

;;; --- live integration (requires node + node-pty) ------------------------

(ert-deftest anvil-pty-broker-test/live-enable-disable-roundtrip ()
  "Broker starts, authenticates, and shuts down cleanly."
  (skip-unless (anvil-pty-broker-test--live-available-p))
  (let ((anvil-pty-broker-port 0)
        (anvil-pty-broker-allowed-commands nil))
    (unwind-protect
        (progn
          (anvil-pty-broker-enable)
          (should (integerp anvil-pty-broker--port))
          (should anvil-pty-broker--authed))
      (anvil-pty-broker-disable))
    (should-not anvil-pty-broker--authed)
    (should-not anvil-pty-broker--net-proc)))

(ert-deftest anvil-pty-broker-test/live-spawn-denied-when-allowlist-empty ()
  "With no allowlist the broker refuses `spawn'."
  (skip-unless (anvil-pty-broker-test--live-available-p))
  (let ((anvil-pty-broker-port 0)
        (anvil-pty-broker-allowed-commands nil))
    (unwind-protect
        (progn
          (anvil-pty-broker-enable)
          (let ((id (anvil-pty-spawn "bash")))
            (anvil-pty-broker--wait-for
             (lambda ()
               (let ((row (gethash id anvil-pty-broker--ptys)))
                 (and row
                      (cl-some (lambda (e)
                                 (and (equal (plist-get e :ev) "error")
                                      (string-match-p "denied"
                                                      (or (plist-get e :message) ""))))
                               (plist-get row :events)))))
             2.0)
            (should-not (plist-get (gethash id anvil-pty-broker--ptys) :pid))))
      (anvil-pty-broker-disable))))

(ert-deftest anvil-pty-broker-test/live-bash-echo-roundtrip ()
  "Spawn bash with allowlist, send `echo hello`, observe output."
  (skip-unless (and (anvil-pty-broker-test--live-available-p)
                    (executable-find "bash")))
  (let ((anvil-pty-broker-port 0)
        (anvil-pty-broker-allowed-commands '("bash")))
    (unwind-protect
        (progn
          (anvil-pty-broker-enable)
          (let ((id (anvil-pty-spawn "bash" :args '("-l"))))
            (anvil-pty-broker--wait-for
             (lambda () (plist-get (gethash id anvil-pty-broker--ptys) :pid))
             2.0)
            (should (plist-get (gethash id anvil-pty-broker--ptys) :pid))
            (anvil-pty-send id "echo hello-world-marker\n")
            (anvil-pty-broker--wait-for
             (lambda ()
               (let ((out (plist-get (gethash id anvil-pty-broker--ptys) :output)))
                 (and out (string-match-p "hello-world-marker" out))))
             3.0)
            (should (string-match-p "hello-world-marker"
                                    (anvil-pty-read id)))
            (anvil-pty-kill id)))
      (anvil-pty-broker-disable))))

(provide 'anvil-pty-broker-test)

;;; anvil-pty-broker-test.el ends here
