;;; http-browser-v1.el --- Cache-hit micro-benchmark for anvil-http + anvil-browser -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton
;; License: GPLv3

;;; Commentary:

;; Measures the wallclock + payload-size delta between a cold fetch
;; (cache miss) and a warm fetch (cache hit within the module's TTL)
;; for `anvil-http-get' and `anvil-browser--tool-fetch'.
;;
;; Usage:
;;   (require 'anvil-http)  (anvil-http-enable)
;;   (require 'anvil-browser) (anvil-browser-enable)
;;   (load-file "benchmarks/http-browser-v1.el")
;;   (anvil-bench-http-browser-run)

;;; Code:

(require 'cl-lib)
(require 'anvil-http)
(require 'anvil-browser)

(defvar anvil-bench-http-browser-urls
  '("https://example.com/"
    "https://raw.githubusercontent.com/torvalds/linux/master/README")
  "URLs that are small, public, and stable enough to exercise both tools.
Both must respond quickly (under ~5s) on a reasonable link — the
bench is not a latency test, the point is comparing miss vs hit.")

(defvar anvil-bench-http-browser-iters 3
  "Iterations per (tool, URL) cell.  First = miss, rest = hit (TTL permitting).")

(defvar anvil-bench-http-browser-results-dir
  (let ((here (file-name-directory
               (or load-file-name buffer-file-name default-directory))))
    (expand-file-name "results" here))
  "Where CSV output lands.")

(defun anvil-bench-http-browser--http-row (url iter)
  "Fire `anvil-http-get' once against URL, return a row plist.
Any error is captured into the row rather than aborting the bench."
  (let* ((t0 (float-time))
         (result (condition-case err
                     (list :ok (anvil-http-get url))
                   (error (list :err (format "%s" err)))))
         (wall-ms (round (* 1000 (- (float-time) t0))))
         (r (plist-get result :ok))
         (body (or (and r (plist-get r :body)) "")))
    (list :tool "http"
          :url url
          :iter iter
          :from-cache (and r (plist-get r :from-cache) t)
          :wall-ms wall-ms
          :body-bytes (length body)
          :status (cond ((plist-get result :err) "error")
                        ((and r (plist-get r :status))
                         (format "%s" (plist-get r :status)))
                        (t "")))))

(defun anvil-bench-http-browser--browser-row (url iter)
  "Fire `anvil-browser--tool-fetch' once against URL, return a row plist.
There is no public `from-cache' flag on the browser tool's return
value; we record the module's internal cache count delta and
derive the hit/miss state from iter order (first = miss by design,
unless a prior session seeded the cache)."
  (let* ((before (anvil-browser--cache-count))
         (t0 (float-time))
         (body (condition-case err
                   (anvil-browser--tool-fetch url)
                 (error (format "ERROR: %s" err))))
         (wall-ms (round (* 1000 (- (float-time) t0))))
         (after (anvil-browser--cache-count)))
    (list :tool "browser"
          :url url
          :iter iter
          ;; cache delta 0 means we served from cache (no new entry)
          :from-cache (= before after)
          :wall-ms wall-ms
          :body-bytes (length (or body ""))
          :status nil)))

(defun anvil-bench-http-browser-run ()
  "Fire the matrix and write a CSV.  Return (:path PATH :rows ROWS).
Errors on any single (tool, URL, iter) become rows with :status \"error\"
instead of aborting the whole run."
  (let (rows)
    (dolist (url anvil-bench-http-browser-urls)
      ;; Fresh cache so iter 1 is a true miss.
      (when (fboundp 'anvil-http--cache-clear-all)
        (anvil-http--cache-clear-all))
      (when (fboundp 'anvil-browser--cache-clear)
        (anvil-browser--cache-clear))
      (dotimes (i anvil-bench-http-browser-iters)
        (push (anvil-bench-http-browser--http-row url (1+ i)) rows))
      (dotimes (i anvil-bench-http-browser-iters)
        (push (condition-case err
                  (anvil-bench-http-browser--browser-row url (1+ i))
                (error (list :tool "browser" :url url :iter (1+ i)
                             :from-cache nil :wall-ms 0 :body-bytes 0
                             :status (format "error:%s" err))))
              rows)))
    (let* ((ordered (nreverse rows))
           (stamp (format-time-string "%Y%m%d-%H%M%S"))
           (path (expand-file-name
                  (format "http-browser-%s.csv" stamp)
                  anvil-bench-http-browser-results-dir))
           (cols '(:tool :url :iter :from-cache :wall-ms :body-bytes :status)))
      (make-directory (file-name-directory path) t)
      (with-temp-file path
        (insert (mapconcat (lambda (k) (substring (symbol-name k) 1)) cols ",")
                "\n")
        (dolist (row ordered)
          (insert (mapconcat
                   (lambda (k)
                     (let ((v (plist-get row k)))
                       (cond ((null v) "")
                             ((eq v t) "t")
                             ((stringp v)
                              (if (string-match-p "[,\"\n]" v)
                                  (concat "\""
                                          (replace-regexp-in-string
                                           "\"" "\"\"" v)
                                          "\"")
                                v))
                             (t (format "%s" v)))))
                   cols ",")
                  "\n")))
      (message "[bench] wrote %s (%d rows)" path (length ordered))
      (list :path path :rows ordered))))

(provide 'anvil-bench-http-browser-v1)

;;; http-browser-v1.el ends here
