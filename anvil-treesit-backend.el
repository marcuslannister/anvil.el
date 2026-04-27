;;; anvil-treesit-backend.el --- Backend abstraction for anvil-ts/js/py  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;;; Commentary:

;; Doc 38 Phase F backend abstraction + Phase G subprocess backend
;; (architecture α pattern).
;;
;; Background: anvil-ts / anvil-js / anvil-py expose MCP tools that
;; structurally analyse TS / JS / Python source.  They were previously
;; thin wrappers over `treesit-*' (= the Emacs builtin tree-sitter
;; binding); that meant they only ran on Emacs, not on NeLisp
;; standalone, and Doc 38 Phase D / E left them with a soft-require
;; on the now-extracted `anvil-ide-treesit.el'.
;;
;; Phase F restored them to first-class citizens of anvil.el by
;; introducing a backend dispatch:
;;
;;     anvil-ts.el / anvil-js.el / anvil-py.el
;;       └─ public API (= MCP tools, unchanged)
;;            └─ internal: anvil-treesit-backend-* helpers
;;                 ├─ if treesit-parser-create available → 'treesit
;;                 │    (Emacs binding; today's main path)
;;                 └─ else                              → 'subprocess
;;                      (Phase G — SHIPPED 2026-04-27)
;;
;; The treesit backend re-exports the legacy `anvil-treesit-*' API
;; (parse / query / node-text / node-range / node-bounds / with-root /
;; compile-query / make-plan / apply-plan / truthy) one-for-one with
;; the same shape and semantics as `anvil-ide-treesit.el', so calling
;; sites in anvil-ts/js/py do not need to change.
;;
;; Phase G ships the four core subprocess ops as real implementations
;; (= python3 -c <inline ast> for Python, `acorn' CLI for JavaScript,
;; acorn fallback for TypeScript / tsx).  When the per-LANG tool is
;; not on PATH the backend signals a structured `:tool-not-found'
;; user-error with an install hint; the public API surface stays
;; backward-compatible and the legacy ensure-grammar / compile-query /
;; with-root paths still signal `:not-implemented-yet' on subprocess
;; (= those need file IO and tree-sitter query DSL, deferred).
;;
;; Backend interface (= Phase G implementors target this):
;;
;;   (anvil-treesit-backend-pick LANG)              → 'treesit / 'subprocess / nil
;;   (anvil-treesit-backend-parse SOURCE LANG)      → AST (backend-shaped)
;;   (anvil-treesit-backend-query AST SELECTOR)     → list of nodes
;;   (anvil-treesit-backend-node-text NODE SOURCE)  → string
;;   (anvil-treesit-backend-node-range NODE)        → (cons START END)
;;
;; The legacy `anvil-treesit-*' helpers (with-root / node-bounds /
;; make-plan / apply-plan / truthy / compile-query) sit on top of the
;; interface and are what the language modules call.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
;; treesit is the Emacs 29+ builtin; on NeLisp standalone it does not
;; exist.  Soft-require keeps load clean; the backend dispatcher will
;; return 'subprocess (= stub) instead.
(require 'treesit nil 'noerror)

(defgroup anvil-treesit nil
  "Tree-sitter backend abstraction for anvil-ts/js/py (Doc 38 Phase F)."
  :group 'anvil
  :prefix "anvil-treesit-")

(defcustom anvil-treesit-language-source-alist
  '((python     "https://github.com/tree-sitter/tree-sitter-python" "v0.21.0")
    (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "v0.21.2"
                "typescript/src")
    (tsx        "https://github.com/tree-sitter/tree-sitter-typescript" "v0.21.2"
                "tsx/src")
    (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "v0.21.4"))
  "Grammar source spec merged into `treesit-language-source-alist' on demand.
Each entry is a list accepted by `treesit-install-language-grammar'
\(LANG URL REVISION SOURCE-DIR CC LIBCXX).  Pinned to revisions
compatible with tree-sitter runtime ABI 14 (Emacs 30 / libtree-sitter
0.22)."
  :type '(alist :key-type symbol
                :value-type (repeat string))
  :group 'anvil-treesit)

(defcustom anvil-treesit-backend-preferred 'auto
  "Backend selection policy.
`auto'       — pick `treesit' when the grammar is loadable, else
               `subprocess' (= stubbed pending Phase G).
`treesit'    — force the in-process tree-sitter backend; signals
               `grammar-missing' when the grammar is not installed.
`subprocess' — force the subprocess backend (= today: stub returning
               `:not-implemented-yet').  Useful for testing the
               dispatch wiring."
  :type '(choice (const auto)
                 (const treesit)
                 (const subprocess))
  :group 'anvil-treesit)

(defvar anvil-treesit--query-cache (make-hash-table :test 'equal)
  "Cache of compiled tree-sitter queries, keyed by (LANG . OP-SYMBOL).
Populated by `anvil-treesit-compile-query', emptied by
`anvil-treesit-clear-query-cache'.")

;;;; --- file → language dispatch -------------------------------------------

(defconst anvil-treesit--extension-alist
  '((python     . ("py" "pyi"))
    (typescript . ("ts"))
    (tsx        . ("tsx"))
    (javascript . ("js" "jsx" "mjs" "cjs")))
  "Mapping from tree-sitter language symbol to file extensions.")

(defun anvil-treesit-language-for-file (file)
  "Return the language symbol for FILE by extension, or nil if unknown."
  (let ((ext (and file (file-name-extension file))))
    (and ext
         (car (cl-find-if (lambda (entry)
                            (member ext (cdr entry)))
                          anvil-treesit--extension-alist)))))

;;;; --- backend dispatch ---------------------------------------------------

(defun anvil-treesit-backend--treesit-available-p (lang)
  "Return non-nil when the treesit backend can serve LANG."
  (and (fboundp 'treesit-parser-create)
       (fboundp 'treesit-language-available-p)
       (treesit-language-available-p lang)))

(defun anvil-treesit-backend-pick (lang)
  "Return the active backend symbol for LANG.
One of `treesit' / `subprocess' / nil.  `nil' means no backend can
serve LANG at all; callers should signal an informative error.
Honours `anvil-treesit-backend-preferred'."
  (pcase anvil-treesit-backend-preferred
    ('treesit
     (and (anvil-treesit-backend--treesit-available-p lang) 'treesit))
    ('subprocess 'subprocess)
    (_
     (cond
      ((anvil-treesit-backend--treesit-available-p lang) 'treesit)
      ;; Subprocess backend is a stub for now (Phase G); we still
      ;; return its symbol so the dispatcher path is exercised end-
      ;; to-end and the caller surfaces the structured stub error.
      (t 'subprocess)))))

;;;; --- grammar availability (treesit backend) -----------------------------

(defun anvil-treesit--install-hint (lang)
  "Return a one-line `M-x' hint for installing LANG."
  (format "M-x treesit-install-language-grammar RET %s RET" lang))

(defun anvil-treesit--source-url (lang)
  "Return the upstream grammar URL for LANG from our pinned source alist."
  (let ((entry (assq lang anvil-treesit-language-source-alist)))
    (and entry (nth 1 entry))))

(defun anvil-treesit-grammar-missing-error (lang)
  "Return a structured `grammar-missing' plist for LANG.
Handlers return this shape as a `user-error' so Claude can relay
the install hint to the user without exposing a backtrace."
  (list :kind 'grammar-missing
        :lang lang
        :install-hint (anvil-treesit--install-hint lang)
        :source-url (anvil-treesit--source-url lang)))

(defun anvil-treesit-ensure-grammar (lang)
  "Ensure a tree-sitter grammar for LANG is loadable on the treesit backend.
Returns t on success.  When the treesit backend itself is unavailable
\(= NeLisp standalone, no `treesit-parser-create'), signals a
structured `:not-implemented-yet' error pointing at Phase G.  When
the backend is available but the grammar is missing, signals the
`grammar-missing' shape used by Doc 21 Phase 1b for Claude relay."
  (cond
   ;; PHASE-G-DEFERRED: ensure-grammar is treesit-only by concept; the
   ;; subprocess backend has nothing to ensure (per-LANG tool detection
   ;; happens at parse time via :tool-not-found).  Surface a consistent
   ;; "not implemented yet" message so the caller's error handler can
   ;; route around it.
   ((not (fboundp 'treesit-language-available-p))
    (anvil-treesit-backend-subprocess-not-implemented lang 'ensure-grammar))
   ((treesit-language-available-p lang) t)
   (t
    (let ((entry (assq lang anvil-treesit-language-source-alist)))
      (when (and entry (boundp 'treesit-language-source-alist))
        (setf (alist-get lang treesit-language-source-alist nil nil #'eq)
              (cdr entry))))
    (user-error "%S" (anvil-treesit-grammar-missing-error lang)))))

;;;; --- query cache --------------------------------------------------------

(defun anvil-treesit-compile-query (lang op source)
  "Compile and cache a tree-sitter query for (LANG . OP).
SOURCE is the query source string.  Returns the compiled query
object; a hit in `anvil-treesit--query-cache' returns immediately.
Signals `:not-implemented-yet' on the subprocess backend (Phase G)."
  (unless (fboundp 'treesit-query-compile)
    (anvil-treesit-backend-subprocess-not-implemented lang 'compile-query))
  (let ((key (cons lang op)))
    (or (gethash key anvil-treesit--query-cache)
        (let ((compiled (treesit-query-compile lang source)))
          (puthash key compiled anvil-treesit--query-cache)
          compiled))))

(defun anvil-treesit-clear-query-cache ()
  "Empty the compiled-query cache.
Call after editing query source in an interactive session."
  (clrhash anvil-treesit--query-cache))

;;;; --- subprocess backend ------------------------------------------------

(defun anvil-treesit-backend-subprocess-not-implemented (lang op)
  "Signal a structured `:not-implemented-yet' user-error for LANG / OP.
Used by entry points (ensure-grammar / compile-query / with-root) that
the subprocess backend does not yet ship as of Doc 38 Phase G.  The
four core dispatch ops (parse / query / node-text / node-range) ship
real subprocess implementations — see
`anvil-treesit-backend-subprocess-parse' etc."
  (user-error
   "%S"
   (list :kind 'not-implemented-yet
         :lang lang
         :op op
         :backend 'subprocess
         :hint (format
                "anvil-treesit-backend: subprocess backend op %S for %S is a Doc 38 \
Phase G TODO (parse/query/node-text/node-range ship; ensure-grammar / compile-query / \
with-root remain stubbed); install Emacs treesit grammar \
(M-x treesit-install-language-grammar RET %s RET) for full coverage"
                op lang lang))))

(defun anvil-treesit-backend-subprocess-tool-not-found (lang tool)
  "Signal a structured `:tool-not-found' user-error for LANG / TOOL.
Surfaces a clear install hint so Claude can relay it to the user
without exposing a backtrace."
  (user-error
   "%S"
   (list :status :tool-not-found
         :lang lang
         :tool tool
         :backend 'subprocess
         :hint (format
                "anvil-treesit-backend: subprocess backend for %S needs %s on PATH \
(install %s for %S support)"
                lang tool tool lang))))

;;;; --- subprocess: per-language tool dispatch ------------------------------

(defcustom anvil-treesit-backend-subprocess-python-executable nil
  "Path to a Python 3 interpreter used by the subprocess backend.
When nil, `executable-find' resolves \"python3\" / \"python\"."
  :type '(choice (const :tag "Auto-detect" nil) (file :must-match t))
  :group 'anvil-treesit)

(defcustom anvil-treesit-backend-subprocess-node-executable nil
  "Path to a Node.js interpreter used by the subprocess backend (acorn fallback).
When nil, `executable-find' resolves \"node\"."
  :type '(choice (const :tag "Auto-detect" nil) (file :must-match t))
  :group 'anvil-treesit)

(defcustom anvil-treesit-backend-subprocess-acorn-executable nil
  "Path to the `acorn' CLI used by the subprocess backend for JS parsing.
When nil, `executable-find' resolves \"acorn\".  Acorn ships
`/usr/bin/acorn' on Debian via the `node-acorn' package, or
globally with `npm install -g acorn'."
  :type '(choice (const :tag "Auto-detect" nil) (file :must-match t))
  :group 'anvil-treesit)

(defcustom anvil-treesit-backend-subprocess-timeout 30
  "Timeout (seconds) for each subprocess parse invocation."
  :type 'integer
  :group 'anvil-treesit)

(defun anvil-treesit-backend--find-python ()
  "Return Python 3 path or nil."
  (or anvil-treesit-backend-subprocess-python-executable
      (executable-find "python3")
      (executable-find "python")))

(defun anvil-treesit-backend--find-acorn ()
  "Return path to `acorn' CLI or nil."
  (or anvil-treesit-backend-subprocess-acorn-executable
      (executable-find "acorn")))

(defun anvil-treesit-backend--find-node ()
  "Return path to `node' or nil."
  (or anvil-treesit-backend-subprocess-node-executable
      (executable-find "node")))

(defconst anvil-treesit-backend--python-script
  "import sys, ast, json
src = sys.stdin.read()
def n2d(n):
    if isinstance(n, ast.AST):
        d = {'type': type(n).__name__}
        for f in n._fields:
            v = getattr(n, f, None)
            d[f] = n2d(v)
        for a in ('lineno', 'col_offset', 'end_lineno', 'end_col_offset'):
            v = getattr(n, a, None)
            if v is not None:
                d[a] = v
        return d
    if isinstance(n, list):
        return [n2d(x) for x in n]
    if isinstance(n, (str, int, float, bool)) or n is None:
        return n
    return repr(n)
def add_offsets(node, cum):
    if not isinstance(node, dict):
        return
    ln = node.get('lineno'); co = node.get('col_offset')
    eln = node.get('end_lineno'); eco = node.get('end_col_offset')
    if ln is not None and co is not None and 0 < ln <= len(cum):
        node['start'] = cum[ln - 1] + co
    if eln is not None and eco is not None and 0 < eln <= len(cum):
        node['end'] = cum[eln - 1] + eco
    for k, v in list(node.items()):
        if isinstance(v, dict):
            add_offsets(v, cum)
        elif isinstance(v, list):
            for x in v:
                if isinstance(x, dict):
                    add_offsets(x, cum)
try:
    tree = ast.parse(src)
    lines = src.split('\\n')
    cum = [0]
    for ln in lines:
        cum.append(cum[-1] + len(ln) + 1)
    d = n2d(tree)
    add_offsets(d, cum)
    sys.stdout.write(json.dumps({'ok': True, 'ast': d}, ensure_ascii=False))
except SyntaxError as e:
    sys.stdout.write(json.dumps({'ok': False, 'error': 'SyntaxError', 'msg': str(e),
                                 'lineno': e.lineno, 'offset': e.offset}))
"
  "Inline Python script that parses stdin source to a JSON AST.
Each node carries `type', `lineno'/`col_offset' and absolute
`start'/`end' (0-based byte offsets); structural children live under
the standard ast field names (`body', `targets', `value', etc).")

(defun anvil-treesit-backend--run-stdin (program args input lang tool)
  "Run PROGRAM with ARGS, feeding INPUT on stdin.  Return stdout string.
Signals `:tool-not-found' with LANG / TOOL on missing PROGRAM, and
`:subprocess-failed' with the captured stderr on non-zero exit."
  (unless program
    (anvil-treesit-backend-subprocess-tool-not-found lang tool))
  (let ((tmp-in (make-temp-file "anvil-treesit-in-"))
        (tmp-out (make-temp-file "anvil-treesit-out-"))
        (tmp-err (make-temp-file "anvil-treesit-err-")))
    (unwind-protect
        (progn
          (let ((coding-system-for-write 'utf-8-unix))
            (with-temp-file tmp-in (insert input)))
          (with-timeout (anvil-treesit-backend-subprocess-timeout
                         (user-error
                          "%S"
                          (list :status :subprocess-timeout
                                :lang lang :tool tool
                                :timeout anvil-treesit-backend-subprocess-timeout)))
            (let* ((exit (with-temp-buffer
                           (let ((coding-system-for-read 'utf-8-unix))
                             (apply #'call-process program tmp-in
                                    (list (list :file tmp-out) tmp-err)
                                    nil args))))
                   (stdout (with-temp-buffer
                             (let ((coding-system-for-read 'utf-8-unix))
                               (insert-file-contents tmp-out))
                             (buffer-string)))
                   (stderr (with-temp-buffer
                             (insert-file-contents tmp-err)
                             (buffer-string))))
              (unless (eql exit 0)
                (user-error "%S"
                            (list :status :subprocess-failed
                                  :lang lang :tool tool
                                  :exit exit
                                  :stderr (string-trim stderr))))
              stdout)))
      (ignore-errors (delete-file tmp-in))
      (ignore-errors (delete-file tmp-out))
      (ignore-errors (delete-file tmp-err)))))

(defun anvil-treesit-backend--parse-python (source)
  "Parse Python SOURCE via `python3 -c <inline ast>'.  Return common AST plist."
  (let* ((py (anvil-treesit-backend--find-python))
         (out (anvil-treesit-backend--run-stdin
               py (list "-c" anvil-treesit-backend--python-script)
               source 'python "python3"))
         (parsed (json-parse-string out
                                    :object-type 'plist
                                    :array-type 'list
                                    :null-object nil
                                    :false-object nil)))
    (cond
     ((eq (plist-get parsed :ok) t)
      (anvil-treesit-backend--normalise-ast
       (plist-get parsed :ast) 'python source))
     (t
      (user-error "%S" (list :status :parse-error
                             :lang 'python
                             :error (plist-get parsed :error)
                             :msg (plist-get parsed :msg)
                             :lineno (plist-get parsed :lineno)))))))

(defun anvil-treesit-backend--parse-javascript (source)
  "Parse JavaScript SOURCE via the `acorn' CLI.  Return common AST plist."
  (let* ((acorn (anvil-treesit-backend--find-acorn)))
    (unless acorn
      (anvil-treesit-backend-subprocess-tool-not-found 'javascript "acorn"))
    (let* ((out (anvil-treesit-backend--run-stdin
                 acorn (list "--ecma2020" "--locations") source
                 'javascript "acorn"))
           (parsed (json-parse-string out
                                      :object-type 'plist
                                      :array-type 'list
                                      :null-object nil
                                      :false-object nil)))
      (anvil-treesit-backend--normalise-ast parsed 'javascript source))))

(defun anvil-treesit-backend--parse-typescript (source lang)
  "Parse TypeScript / TSX SOURCE.  LANG is `typescript' or `tsx'.
Tries `tsc' / tree-sitter CLI / Babel in order, falling back to
`acorn' (= JS-only subset) when no TS-aware tool is on PATH.  When
`acorn' is absent too, signals `:tool-not-found' with the full list
of tried tools so the user can install one."
  (cond
   ((executable-find "tsc")
    ;; TODO Phase G+1: full ts-morph / tsc --noEmit AST extraction
    ;; via a Node helper.  For now we degrade to acorn (which loses
    ;; type annotations but keeps structural shape).
    (anvil-treesit-backend--parse-typescript-via-acorn source lang))
   ((anvil-treesit-backend--find-acorn)
    (anvil-treesit-backend--parse-typescript-via-acorn source lang))
   (t
    (anvil-treesit-backend-subprocess-tool-not-found
     lang "tsc or acorn"))))

(defun anvil-treesit-backend--parse-typescript-via-acorn (source lang)
  "Parse TypeScript SOURCE through the `acorn' CLI.
LANG is `typescript' or `tsx'.  This is a deliberate degraded path
when no real TS tool is on PATH; type-only constructs may produce
syntax errors.  Returns the common AST plist on success."
  (let* ((acorn (anvil-treesit-backend--find-acorn)))
    (unless acorn
      (anvil-treesit-backend-subprocess-tool-not-found lang "acorn"))
    (let* ((out (anvil-treesit-backend--run-stdin
                 acorn (list "--ecma2020" "--locations") source
                 lang "acorn"))
           (parsed (json-parse-string out
                                      :object-type 'plist
                                      :array-type 'list
                                      :null-object nil
                                      :false-object nil)))
      (anvil-treesit-backend--normalise-ast parsed lang source))))

;;;; --- subprocess: AST normalisation --------------------------------------

(defun anvil-treesit-backend--normalise-ast (raw lang source)
  "Convert RAW (parser-specific JSON plist) into the common AST shape.
Common shape:
  (:type STRING
   :start INTEGER       ; 0-based offset into SOURCE
   :end   INTEGER       ; 0-based exclusive
   :name  STRING-OR-NIL
   :children LIST
   :backend `subprocess
   :lang LANG
   :raw  PLIST          ; original parser plist for advanced callers)
The caller supplies SOURCE so that text-extraction queries do not
need a second round-trip to the subprocess."
  (let ((normalised (anvil-treesit-backend--walk-normalise raw)))
    (list :type (plist-get normalised :type)
          :start (plist-get normalised :start)
          :end (plist-get normalised :end)
          :name (plist-get normalised :name)
          :children (plist-get normalised :children)
          :backend 'subprocess
          :lang lang
          :source source
          :raw raw)))

(defun anvil-treesit-backend--node-plistp (x)
  "Return non-nil when X is a non-empty plist with a `:type' key.
Used to guard recursion: a literal nil is the empty plist (= matches
`plistp') but is not a node we want to descend into."
  (and (consp x)
       (plistp x)
       (or (plist-member x :type) (plist-member x :_type))))

(defun anvil-treesit-backend--walk-normalise (node)
  "Recursively normalise NODE (parser-shaped plist) to common shape.
Handles Python (`type', `start' synthesised) and Acorn (`type',
`start', `end', `body'/`declarations'/etc) using unified key
extraction.  Returns NODE unchanged when it is not a node-shaped
plist (= guards against descending into nil / scalar values)."
  (cond
   ((not (anvil-treesit-backend--node-plistp node)) node)
   (t
    (let* ((type (or (plist-get node :type) (plist-get node :_type)))
           (start (plist-get node :start))
           (end (plist-get node :end))
           (name (anvil-treesit-backend--extract-name node))
           (kids (anvil-treesit-backend--extract-children node)))
      (list :type (if (symbolp type) (symbol-name type) type)
            :start start
            :end end
            :name name
            :children (delq nil
                            (mapcar #'anvil-treesit-backend--walk-normalise
                                    kids)))))))

(defun anvil-treesit-backend--extract-name (node)
  "Return identifier-ish name for NODE, or nil.
Handles Acorn's `:id (:type \"Identifier\" :name \"foo\")', Python's
`:name \"foo\"' / `:id \"x\"' patterns."
  (let ((id (plist-get node :id))
        (name (plist-get node :name)))
    (cond
     ((stringp name) name)
     ((and (plistp id) (stringp (plist-get id :name)))
      (plist-get id :name))
     ((stringp (plist-get node :id)) (plist-get node :id))
     (t nil))))

(defun anvil-treesit-backend--extract-children (node)
  "Collect child plist nodes from NODE under common parser keys.
Acorn uses `:body', `:declarations', `:expression', `:left', `:right',
`:argument', `:arguments', `:properties', `:elements'.  Python uses
`:body', `:targets', `:value', `:args', `:test', `:orelse'."
  (let (kids)
    (dolist (key '(:body :declarations :expression :left :right :argument
                   :arguments :properties :elements :targets :value
                   :args :test :orelse :consequent :alternate :params
                   :init :update :handlers :finalbody))
      (let ((v (plist-get node key)))
        (cond
         ((anvil-treesit-backend--node-plistp v) (push v kids))
         ((listp v)
          (dolist (x v)
            (when (anvil-treesit-backend--node-plistp x) (push x kids)))))))
    (nreverse kids)))

;;;; --- subprocess: parse / query / node-text / node-range ----------------

(defun anvil-treesit-backend-subprocess-parse (source lang)
  "Parse SOURCE for LANG via subprocess.  Return the common AST plist.
LANG is one of `python' / `javascript' / `typescript' / `tsx'.
Signals `:tool-not-found' when the per-LANG tool is not on PATH,
`:parse-error' on syntax errors, `:subprocess-failed' on non-zero
exit, `:subprocess-timeout' on hang.  Empty SOURCE returns a root
node with empty `:children'."
  (let ((src (or source "")))
    (pcase lang
      ('python (anvil-treesit-backend--parse-python src))
      ('javascript (anvil-treesit-backend--parse-javascript src))
      ((or 'typescript 'tsx)
       (anvil-treesit-backend--parse-typescript src lang))
      (_
       (user-error "%S"
                   (list :status :unsupported-lang
                         :lang lang
                         :backend 'subprocess
                         :hint
                         "anvil-treesit-backend subprocess: only python / \
javascript / typescript / tsx are supported"))))))

(defun anvil-treesit-backend-subprocess-query (ast selector)
  "Walk AST (= common-shape plist) and return nodes matching SELECTOR.
SELECTOR is either a string node-type (e.g., \"FunctionDeclaration\")
or a plist `(:type STRING)' that may be extended in later phases.
Always returns a list (possibly empty); never signals on a miss."
  (let ((target (cond
                 ((stringp selector) selector)
                 ((and (plistp selector) (plist-get selector :type))
                  (plist-get selector :type))
                 (t nil)))
        acc)
    (anvil-treesit-backend--walk-collect ast target (lambda (n) (push n acc)))
    (nreverse acc)))

(defun anvil-treesit-backend--walk-collect (node target visit)
  "Recursively walk NODE; call VISIT on each node whose `:type' = TARGET."
  (when (and (plistp node) (plist-get node :type))
    (when (or (null target)
              (string= target (plist-get node :type)))
      (funcall visit node))
    (dolist (kid (plist-get node :children))
      (anvil-treesit-backend--walk-collect kid target visit))))

(defun anvil-treesit-backend-subprocess-node-text (node &optional source)
  "Return NODE's source text by slicing SOURCE on `:start'..`:end'.
SOURCE defaults to the `:source' captured by `…-parse'.  Returns
nil when offsets are missing (= the parser did not record positions)."
  (let* ((src (or source (plist-get node :source)))
         (start (plist-get node :start))
         (end (plist-get node :end)))
    (when (and (stringp src) (integerp start) (integerp end)
               (<= 0 start) (<= end (length src)))
      (substring src start end))))

(defun anvil-treesit-backend-subprocess-node-range (node)
  "Return (START . END) for NODE, or nil when offsets are missing."
  (let ((s (plist-get node :start))
        (e (plist-get node :end)))
    (and (integerp s) (integerp e) (cons s e))))

;;;; --- treesit backend implementation -------------------------------------

(defun anvil-treesit-backend-treesit-parse (source lang)
  "Parse SOURCE (a string) with the treesit backend for LANG.
Returns the root node.  Internal helper — the documented user-
facing entry is `anvil-treesit-with-root', which handles file IO."
  (anvil-treesit-ensure-grammar lang)
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8-unix))
      (insert source))
    (let ((parser (treesit-parser-create lang)))
      (treesit-parser-root-node parser))))

(defun anvil-treesit-backend-treesit-query (root selector)
  "Run SELECTOR (compiled query) against ROOT.  Returns capture alist."
  (treesit-query-capture root selector))

(defun anvil-treesit-backend-treesit-node-text (node &optional _source)
  "Return NODE's text content as a string."
  (treesit-node-text node t))

(defun anvil-treesit-backend-treesit-node-range (node)
  "Return (START . END) for NODE, 1-based buffer positions."
  (cons (treesit-node-start node) (treesit-node-end node)))

;;;; --- backend interface (= dispatch) -------------------------------------

(defun anvil-treesit-backend-parse (source lang)
  "Parse SOURCE for LANG with the active backend.
Dispatches via `anvil-treesit-backend-pick'.  On the subprocess
backend, signals a structured `:not-implemented-yet' error
\(Phase G).  On the treesit backend, returns the root node."
  (pcase (anvil-treesit-backend-pick lang)
    ('treesit (anvil-treesit-backend-treesit-parse source lang))
    ('subprocess (anvil-treesit-backend-subprocess-parse source lang))
    (_ (anvil-treesit-backend-subprocess-not-implemented lang 'parse))))

(defun anvil-treesit-backend-query (ast selector)
  "Run SELECTOR against AST.
Dispatches: assumes the AST shape carries enough info for the
backend; the treesit backend returns `treesit-query-capture' results."
  ;; treesit nodes are opaque; we infer the backend from the node
  ;; predicate.  Subprocess ASTs would be plain plists.
  (cond
   ((and (fboundp 'treesit-node-p) (treesit-node-p ast))
    (anvil-treesit-backend-treesit-query ast selector))
   (t (anvil-treesit-backend-subprocess-query ast selector))))

(defun anvil-treesit-backend-node-text (node &optional source)
  "Return NODE's text.  SOURCE is needed for subprocess nodes only."
  (cond
   ((and (fboundp 'treesit-node-p) (treesit-node-p node))
    (anvil-treesit-backend-treesit-node-text node source))
   (t (anvil-treesit-backend-subprocess-node-text node source))))

(defun anvil-treesit-backend-node-range (node)
  "Return (START . END) for NODE."
  (cond
   ((and (fboundp 'treesit-node-p) (treesit-node-p node))
    (anvil-treesit-backend-treesit-node-range node))
   (t (anvil-treesit-backend-subprocess-node-range node))))

;;;; --- file → parser tree (legacy helpers, treesit backend) ---------------

(defun anvil-treesit--insert-file (file)
  "Insert FILE into the current buffer as UTF-8.
Used inside `with-temp-buffer' from `anvil-treesit-with-root'.
Forces UTF-8 so the tree-sitter input bytes align with the grammar's
UTF-8 expectation regardless of the user's locale."
  (let ((coding-system-for-read 'utf-8-unix))
    (insert-file-contents file)))

(defun anvil-treesit-with-root-fn (file lang fn)
  "Functional form of `anvil-treesit-with-root'.
Opens FILE, creates a parser for LANG via the active backend, and
calls FN with the root node.  Signals a structured `grammar-missing'
or `:not-implemented-yet' `user-error' depending on which backend
is unavailable.  FILE may not be nil."
  (unless (and file (stringp file))
    (user-error "anvil-treesit: FILE must be a string, got %S" file))
  (unless (file-readable-p file)
    (user-error "anvil-treesit: cannot read file %s" file))
  (pcase (anvil-treesit-backend-pick lang)
    ('treesit
     (anvil-treesit-ensure-grammar lang)
     (with-temp-buffer
       (anvil-treesit--insert-file file)
       (let* ((parser (treesit-parser-create lang))
              (root (treesit-parser-root-node parser)))
         (funcall fn root))))
    ('subprocess
     (anvil-treesit-backend-subprocess-not-implemented lang 'with-root))
    (_
     (anvil-treesit-backend-subprocess-not-implemented lang 'with-root))))

(defmacro anvil-treesit-with-root (file lang var &rest body)
  "Bind VAR to the root node of FILE parsed with LANG, then run BODY.
Macro form of `anvil-treesit-with-root-fn'.  The parser and buffer
are torn down when BODY returns."
  (declare (indent 3) (debug (form form symbolp body)))
  `(anvil-treesit-with-root-fn ,file ,lang (lambda (,var) ,@body)))

;;;; --- node helpers (legacy, treesit-shaped) ------------------------------

(defun anvil-treesit-node-range (node)
  "Return (:start POINT :end POINT) for NODE as 1-based point positions."
  (list :start (treesit-node-start node)
        :end (treesit-node-end node)))

(defun anvil-treesit-node-text (node)
  "Return NODE's text content as a string."
  (treesit-node-text node t))

(defun anvil-treesit-node-bounds (node)
  "Return (:start :end :start-line :end-line) for NODE.
Line numbers are 1-based.  Convenience shape for listing operations."
  (let ((s (treesit-node-start node))
        (e (treesit-node-end node)))
    (list :start s
          :end e
          :start-line (line-number-at-pos s)
          :end-line (line-number-at-pos e))))

(defun anvil-treesit-child-by-field (node field-name)
  "Return NODE's child under FIELD-NAME, or nil."
  (treesit-node-child-by-field-name node field-name))

;;;; --- edit-plan primitives ------------------------------------------------

(defun anvil-treesit-truthy (v)
  "Return non-nil when V is a truthy MCP value."
  (not (or (null v)
           (eq v :json-false)
           (eq v :false)
           (and (stringp v)
                (member v '("" "nil" "false" "0" "no" "False" "NIL"))))))

(defun anvil-treesit--unified-diff (file beg end replacement)
  "Return a short unified-diff string for FILE swapping [BEG,END) with REPLACEMENT."
  (let* ((old (with-temp-buffer
                (insert-file-contents file)
                (buffer-substring-no-properties beg end)))
         (base (file-name-nondirectory file))
         (lines-old (split-string old "\n"))
         (lines-new (split-string replacement "\n"))
         (out (list (format "+++ %s (after)" base)
                    (format "--- %s (before)" base))))
    (dolist (l lines-old)
      (push (concat "-" l) out))
    (dolist (l lines-new)
      (push (concat "+" l) out))
    (mapconcat #'identity (nreverse out) "\n")))

(defun anvil-treesit-make-plan (file beg end replacement reason)
  "Build an edit plan for a single file / single range."
  (list :ops (list (list :file file
                         :range (cons beg end)
                         :replacement replacement
                         :reason reason))
        :summary (format "%s: 1 op on %s" reason
                         (file-name-nondirectory file))
        :diff-preview (anvil-treesit--unified-diff file beg end replacement)))

(defun anvil-treesit-make-noop-plan (file reason)
  "Build an empty plan noting that REASON is already satisfied in FILE."
  (list :ops nil
        :summary (format "%s: no-op (already satisfied) on %s"
                         reason (file-name-nondirectory file))
        :diff-preview ""))

(defun anvil-treesit--assert-ops-non-overlapping (ops)
  "Signal an error when any two OPS on the same file overlap."
  (let ((by-file (make-hash-table :test 'equal)))
    (dolist (op ops)
      (push op (gethash (plist-get op :file) by-file)))
    (maphash
     (lambda (file file-ops)
       (let ((sorted (sort (copy-sequence file-ops)
                           (lambda (a b)
                             (< (car (plist-get a :range))
                                (car (plist-get b :range)))))))
         (cl-loop for (a b) on sorted while b do
                  (let ((a-end (cdr (plist-get a :range)))
                        (b-beg (car (plist-get b :range))))
                    (when (> a-end b-beg)
                      (error "anvil-treesit: overlapping ops in %s: %S vs %S"
                             file (plist-get a :range)
                             (plist-get b :range)))))))
     by-file)))

(defun anvil-treesit-apply-plan (plan)
  "Write every op in PLAN to disk.  Return PLAN with :applied-at added."
  (let* ((ops (plist-get plan :ops))
         (by-file (make-hash-table :test 'equal)))
    (when ops
      (anvil-treesit--assert-ops-non-overlapping ops)
      (dolist (op ops)
        (push op (gethash (plist-get op :file) by-file)))
      (maphash
       (lambda (file file-ops)
         (let ((sorted (sort (copy-sequence file-ops)
                             (lambda (a b)
                               (> (car (plist-get a :range))
                                  (car (plist-get b :range)))))))
           (with-temp-buffer
             (let ((coding-system-for-read 'utf-8-unix)
                   (coding-system-for-write 'utf-8-unix))
               (insert-file-contents file)
               (dolist (op sorted)
                 (let ((r (plist-get op :range)))
                   (delete-region (car r) (cdr r))
                   (goto-char (car r))
                   (insert (plist-get op :replacement))))
               (write-region (point-min) (point-max) file nil 'silent)))))
       by-file))
    (append plan (list :applied-at (format-time-string "%FT%T%z")))))

;;;; --- module lifecycle ---------------------------------------------------

(defun anvil-treesit-backend-enable ()
  "Enable the anvil-treesit backend layer.
No MCP tools are registered at the backend level — per-language
modules (anvil-py, anvil-ts, anvil-js) own their own surface.
Provided so `anvil--load-module' can discover `treesit-backend' as
a first-class module."
  (interactive)
  t)

(defun anvil-treesit-backend-disable ()
  "Disable the anvil-treesit backend layer.  Clears the query cache."
  (interactive)
  (anvil-treesit-clear-query-cache)
  t)

(provide 'anvil-treesit-backend)
;;; anvil-treesit-backend.el ends here
