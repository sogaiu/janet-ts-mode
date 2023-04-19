;;; janet-ts-mode.el --- A major mode for Janet -*- lexical-binding: t; -*-
;;
;; Author: sogaiu
;; Version: 0.0.1
;; URL: https://github.com/sogaiu/janet-ts-mode
;; Package-Requires: ((emacs "29.0"))
;;
;;; Status:
;;
;; Proof-of-concept, but more than a few things seem to work somewhat :)
;;
;;; Commentary:
;;
;; Defines a tree-sitter-based major mode for the Janet programming language
;;
;; What Might Get Covered:
;;
;; * Highlighting (fair bit working)
;; * Indentation (fair bit working)
;; * Imenu (somewhat functional)
;; * Navigation (somewhat functional)
;; * Which-Func (somewhat functional)
;; * Syntax Table Stuff (some bits implemented)
;;
;; Dependencies:
;;
;; * tree-sitter
;;     https://github.com/tree-sitter/tree-sitter
;; * Emacs (tested with commit 5325d815)
;;     https://github.com/emacs-mirror/emacs
;; * tree-sitter-module
;;     https://github.com/casouri/tree-sitter-module
;;
;; Detailed setup instructions at:
;;   https://gist.github.com/sogaiu/a13512e146e8f5c0e94d1804838558ee
;;
;; Starter Guide On Writing Major Mode With Tree-Sitter (aka starter)
;;   <emacs-repository-root>/admin/notes/tree-sitter/starter-guide
;;
;; Things to try, once this buffer has been successfully evaluated
;;
;; * Open a .janet file, then:
;;   * M-x treesit-end-of-defun
;;   * M-x treesit-beginning-of-defun
;;   * M-x imenu
;;   * indenting
;;   * observing syntax highligthing
;;
;; * Developer-ish things:
;;   * M-x treesit-explore-mode
;;   * M-: (treesit-query-validate ...)
;;   * M-: (treesit-query-string ...)

;;; TODO:
;;
;; * go through XXX in this file to determine what needs attention :)
;; * think about how to keep various regexps up-to-date with language changes
;; * think about how to handle jpm's project.janet
;;   * would adding everything from jpm, e.g. post-deps, rule, phony, etc.
;;     for highligthing be too much?
;;   * better to have separate handling?
;;   * somehow have highlighting be different just for project.janet?
;; * consider whether attending to destructuring defs for imenu is worth it
;; * investigate weird behavior concerning treesit-end-of-defun for do, etc.

;;; Code:
;;
;; According to `(elisp) Language Definitions', language-specific grammar
;; .so files are searched in 3 types of places:
;;
;; 1. list of directories specified by treesit-extra-load-path, so e.g.
;;
;;    (setq treesit-extra-load-path '("~/src/tree-sitter-module/dist"))
;;
;; 2. in tree-sitter subdirectory of user-emacs-directory -- this
;;    might end up being something like ~/.emacs.d/tree-sitter/ -- an
;;    appropriate symlink to ~/src/tree-sitter-module/dist seems to
;;    work (assuming tree-sitter-module has been cloned under ~/src/)
;;
;; 3. system's default locations for dynamic libraries

(require 'treesit)
;; XXX
;;(eval-when-compile (require 'rx))

;; XXX: is this really necessary?
;; one for each treesit-* c function used in this buffer
(declare-function treesit-available-p "treesit.c")
(declare-function treesit-node-child "treesit.c")
(declare-function treesit-node-child-count "treesit.c")
(declare-function treesit-node-end "treesit.c")
(declare-function treesit-node-next-sibling "treesit.c")
(declare-function treesit-node-parent "treesit.c")
(declare-function treesit-node-start "treesit.c")
(declare-function treesit-node-text "treesit.c")
(declare-function treesit-node-type "treesit.c")
(declare-function treesit-parser-create "treesit.c")
(declare-function treesit-query-capture "treesit.c")
(declare-function treesit-query-compile "treesit.c")

(defvar janet-ts--syntax-table
  (let ((table (make-syntax-table)))
    ;; comments
    (modify-syntax-entry ?# "<" table)
    (modify-syntax-entry ?\n ">" table)
    ;;
    (modify-syntax-entry ?! "_" table)
    (modify-syntax-entry ?. "_" table)
    (modify-syntax-entry ?: "_" table)
    (modify-syntax-entry ?? "_" table)
    (modify-syntax-entry ?^ "_" table)
    ;;
    (modify-syntax-entry ?| "'" table)
    (modify-syntax-entry ?\; "'" table)
    (modify-syntax-entry ?' "'" table)
    (modify-syntax-entry ?~ "'" table)
    (modify-syntax-entry ?, "'" table)
    (modify-syntax-entry ?@ "'" table)
    ;;
    table)
  ;;
  "Syntax table for `janet-ts-mode'.")

;; some aliases for keywords (see defdyn definition in boot.janet)
(defconst janet-ts--builtin-dynamic-regexp
  (eval-and-compile
    (concat "^"
            (regexp-opt
             '("*args*"
               "*current-file*"
               "*debug*" "*defdyn-prefix*" "*doc-color*" "*doc-width*"
               "*err*" "*err-color*" "*executable*" "*exit*" "*exit-value*"
               "*ffi-context*"
               "*lint-error*" "*lint-levels*" "*lint-warn*"
               "*macro-form*" "*macro-lints*"
               "*out*"
               "*peg-grammar*" "*pretty-format*" "*profilepath*"
               "*redef*"
               "*syspath*"))
            "$")))

;; see a-janet-mode's highlights/default.scm
(defconst janet-ts--builtin-value-regexp
  (eval-and-compile
    (concat "^"
            (regexp-opt
             '("debugger-env" "default-peg-grammar"
               "janet/build" "janet/config-bits" "janet/version"
               "load-image-dict"
               "make-image-dict"
               "math/-inf" "math/e" "math/inf" "math/int-max" "math/int-min"
               "math/int32-max" "math/int32-min" "math/nan"
               "module/cache" "module/loaders" "module/loading"
               "module/paths"
               "root-env"
               "stderr" "stdin" "stdout"))
            "$")))

;; see jpm's source code
(defconst janet-ts--jpm-builtin-value-regexp
  (eval-and-compile
    (concat "^"
            (regexp-opt
             '("declare-archive" "declare-bin" "declare-binscript"
               "declare-executable" "declare-headers" "declare-manpage"
               "declare-native" "declare-project" "declare-source"
               "install-file-rule" "install-rule"
               "run-repl" "run-script" "run-tests"
               "uninstall"))
            "$")))

;; see a-janet-mode's highlights/default.scm
(defconst janet-ts--special-form-regexp
  (eval-and-compile
    (concat "^"
            (regexp-opt
             '("break"
               "def" "do"
               "fn"
               "if"
               "quasiquote" "quote"
               "set" "splice"
               "unquote" "upscope"
               "var" "varfn"
               "while"))
            "$")))

;; see a-janet-mode's highlights/default.scm
(defconst janet-ts--builtin-macro-regexp
  (eval-and-compile
    (concat "^"
            (regexp-opt
             '("++" "+=" "--"
               "->" "->>" "-?>" "-?>>"
               "-=" "*=" "/=" "%="
               "and" "as->" "as?->"
               "case" "chr" "comment" "compif" "comptime" "compwhen" "cond"
               "coro"
               "def-" "default" "defdyn" "defer" "defmacro" "defmacro-"
               "defn" "defn-"
               "delay" "doc"
               "each" "eachk" "eachp"
               "eachy" ;; XXX: obsolete
               "edefer"
               "ev/do-thread" "ev/gather" "ev/spawn" "ev/spawn-thread"
               "ev/with-deadline"
               "ffi/defbind"
               "for" "forever" "forv"
               "generate"
               "if-let" "if-not" "if-with" "import"
               "juxt"
               "label" "let" "loop"
               "match"
               "or"
               "prompt" "protect"
               "repeat"
               "seq" "short-fn"
               "tabseq" "tracev" "try"
               "unless" "use"
               "var-"
               "when" "when-let" "when-with" "with" "with-dyns" "with-syms"
               "with-vars"))
            "$")))

;; see a-janet-mode's highlights/default.scm
(defconst janet-ts--builtin-function-regexp
  (eval-and-compile
    (concat "^"
            (regexp-opt
             '("*" "+" "-" "/"
               "<" "<=" "=" ">" ">="
               ".break" ".breakall" ".bytecode"
               ".clear" ".clearall"
               ".disasm"
               ".fiber"
               ".fn" ".frame"
               ".next" ".nextc"
               ".ppasm"
               ".signal" ".slot" ".slots" ".source" ".stack" ".step"
               "abstract?" "accumulate" "accumulate2" "all" "all-bindings"
               "all-dynamics" "any?" "apply" "array" "array?"
               "array/clear" "array/concat" "array/ensure" "array/fill"
               "array/insert" "array/new" "array/new-filled" "array/peek"
               "array/pop" "array/push" "array/remove" "array/slice"
               "array/trim"
               "asm" "assert"
               "bad-compile" "bad-parse" "band" "blshift" "bnot" "boolean?"
               "bor" "brshift" "brunshift" "buffer"
               "buffer/bit" "buffer/bit-clear" "buffer/bit-set"
               "buffer/bit-toggle" "buffer/blit" "buffer/clear"
               "buffer/fill" "buffer/format" "buffer/new-filled"
               "buffer/popn" "buffer/push" "buffer/push-byte"
               "buffer/push-string" "buffer/push-word" "buffer/slice"
               "buffer/trim"
               "buffer?" "bxor" "bytes?"
               "cancel" "cfunction?" "cli-main" "cmp" "comp" "compare"
               "compare<" "compare<=" "compare=" "compare>" "compare>="
               "compile" "complement" "count" "curenv"
               "debug"
               "debug/arg-stack" "debug/break" "debug/fbreak" "debug/lineage"
               "debug/stack" "debug/stacktrace" "debug/step" "debug/unbreak"
               "debug/unfbreak"
               "dec" "deep=" "deep-not=" "defglobal" "describe" "dictionary?"
               "disasm" "distinct" "doc*" "doc-format" "dofile" "drop"
               "drop-until" "drop-while" "dyn"
               "eflush" "empty?" "env-lookup" "eprin" "eprinf" "eprint"
               "eprintf" "error" "errorf"
               "ev/call" "ev/close" "ev/capacity" "ev/chan" "ev/chunk"
               "ev/close" "ev/count" "ev/deadline" "ev/full" "ev/give"
               "ev/go" "ev/read" "ev/rselect" "ev/select" "ev/sleep"
               "ev/take" "ev/write"
               "eval" "eval-string" "even?" "every?" "extreme"
               "false?"
               "ffi/align" "ffi/call" "ffi/close" "ffi/context" "ffi/free"
               "ffi/jitfn" "ffi/lookup" "ffi/malloc" "ffi/native"
               "ffi/read" "ffi/signature" "ffi/size" "ffi/struct"
               "ffi/trampoline" "ffi/write"
               "fiber/can-resume?" "fiber/current" "fiber/getenv"
               "fiber/maxstack" "fiber/new" "fiber/root" "fiber/setenv"
               "fiber/setmaxstack" "fiber/status"
               "fiber?"
               "file/close" "file/flush" "file/open" "file/popen" "file/read"
               "file/seek" "file/tell" "file/temp" "file/write"
               "filter" "find" "find-index" "first" "flatten" "flatten-into"
               "flush" "flycheck" "freeze" "frequencies" "function?"
               "gccollect" "gcinterval" "gcsetinterval" "gensym" "get"
               "get-in" "getline"
               "hash"
               "idempotent?" "identity" "import*" "in" "inc" "index-of"
               "indexed?"
               "int/s64" "int/u64"
               "int?"
               "interleave" "interpose" "invert"
               "juxt*"
               "keep" "keys"
               "keyword"
               "keyword/slice"
               "keyword?"
               "kvs"
               "last" "length" "load-image"
               "macex" "macex1"
               "make-env" "make-image" "map" "mapcat" "marshal"
               "math/abs" "math/acos" "math/acosh" "math/asin"
               "math/asinh" "math/atan" "math/atan2" "math/atanh" "math/cbrt"
               "math/ceil" "math/cos" "math/cosh" "math/erf"
               "math/erfc" "math/exp" "math/exp2" "math/expm1" "math/floor"
               "math/gamma" "math/hypot" "math/log" "math/log10" "math/log1p"
               "math/log2" "math/next" "math/pi" "math/pow" "math/random"
               "math/rng" "math/rng-buffer" "math/rng-int" "math/rng-uniform"
               "math/round" "math/seedrandom" "math/sin" "math/sinh"
               "math/sqrt" "math/tan" "math/tanh" "math/trunc"
               "max" "mean" "merge" "merge-into" "merge-module" "min" "mod"
               "module/add-paths" "module/expand-path" "module/find"
               "nan?" "nat?" "native" "neg?"
               "net/accept" "net/accept-loop" "net/address" "net/chunk"
               "net/close" "net/connect" "net/flush" "net/listen" "net/read"
               "net/recv-from" "net/send-to" "net/server" "net/write"
               "next" "nil?" "not" "not=" "number?"
               "odd?" "one?"
               "os/arch" "os/cd" "os/chmod" "os/clock" "os/compiler"
               "os/cpu-count" "os/cryptorand" "os/cwd" "os/date" "os/dir"
               "os/environ" "os/execute" "os/exit" "os/getenv" "os/link"
               "os/lstat" "os/mkdir" "os/mktime" "os/open" "os/perm-int"
               "os/perm-string" "os/pipe" "os/proc-close" "os/proc-kill"
               "os/proc-wait" "os/readlink" "os/realpath" "os/rename"
               "os/rm" "os/rmdir" "os/setenv" "os/shell" "os/sleep"
               "os/spawn" "os/stat" "os/symlink" "os/time" "os/touch"
               "os/umask" "os/which"
               "pairs" "parse"
               "parser/byte" "parser/clone" "parser/consume" "parser/eof"
               "parser/error" "parser/flush" "parser/has-more"
               "parser/insert" "parser/new" "parser/produce" "parser/state"
               "parser/status" "parser/where"
               "partial" "partition"
               "peg/compile" "peg/find" "peg/find-all" "peg/match"
               "peg/replace" "peg/replace-all"
               "pos?" "postwalk" "pp" "prewalk" "prin" "prinf" "print"
               "printf" "product" "propagate" "put" "put-in"
               "quit"
               "range" "reduce" "reduce2" "repl" "require" "resume"
               "return" "reverse" "reverse!" "run-context"
               "scan-number" "setdyn" "signal" "slice" "slurp" "some" "sort"
               "sort-by" "sorted" "sorted-by" "spit"
               "string"
               "string/ascii-lower" "string/ascii-upper" "string/bytes"
               "string/check-set" "string/find" "string/find-all"
               "string/format" "string/from-bytes" "string/has-prefix?"
               "string/has-suffix?" "string/join" "string/repeat"
               "string/replace" "string/replace-all" "string/reverse"
               "string/slice" "string/split" "string/trim" "string/triml"
               "string/trimr"
               "string?"
               "struct"
               "struct/getproto" "struct/proto-flatten" "struct/to-table"
               "struct/with-proto"
               "struct?"
               "sum"
               "symbol"
               "symbol/slice"
               "symbol?"
               "table"
               "table/clone" "table/getproto" "table/new" "table/rawget"
               "table/setproto" "table/to-struct"
               "table?"
               "take" "take-until" "take-while"
               ;; XXX: obsolete
               "tarray/buffer" "tarray/copy-bytes" "tarray/length"
               "tarray/new" "tarray/properties" "tarray/slice"
               "tarray/swap-bytes"
               ;; XXX: obsolete
               "thread/close" "thread/current" "thread/exit" "thread/new"
               "thread/receive" "thread/send"
               ;; resume non-obsolete
               "trace" "true?" "truthy?"
               "tuple"
               "tuple/brackets" "tuple/setmap" "tuple/slice"
               "tuple/sourcemap" "tuple/type"
               "tuple?"
               "type"
               "unmarshal" "untrace" "update" "update-in"
               "values" "varglobal"
               "walk"
               "xprin" "xprinf" "xprint" "xprintf"
               "yield"
               "zero?" "zipcoll"))
            "$")))

(defconst janet-ts--definition-keyword-regexp
  (rx (or (group line-start (or "def" "fn" "var") line-end)
          ;; XXX: line-start and line-end?
          (group line-start
                 (or "def" "var")
                 ;; XXX: probably needs work
                 (+ (or alnum
                        "!" "$" "%" "&" "*" "+" "-" "." "/"
                        ":" "<" "?" "=" ">" "@" "^" "_"))))))

(eval-and-compile
  (defconst janet-ts--slashed-symbol-regexp
    "^\\([^/]+\\)/"
    "A regex matching a symbol name up through the first slash.
Captures the portion of the symbol before the slash in the first group."))

;; to debug font-lock stuff, before opening a .janet file:
;;
;;   (setq font-lock-support-mode nil)
;;
;; subsequently, the results of `message` should be visible (edebug stepping
;; should work too)

;; XXX: consider a version that looks for the last slash that isn't the
;;      the last char (so hi// would select hi, but hi/a/b would select hi/a)
(defun janet-ts--fontify-slashed-symbol (node _override start end &rest _)
  "Fontify symbols that have at least one slash.
For NODE, OVERRIDE, START, and END see `treesit-font-lock-rules'."
  (let ((sym-text (treesit-node-text node 'no-prop))
        (sym-start (treesit-node-start node)))
    ;; XXX
    ;; (message "----------------")
    ;; (message "sym-text: %s" sym-text)
    ;; (message "override: %s" override)
    ;; (message "start: %s" start)
    ;; (message "end: %s" end)
    ;; (message "sym-start: %s" sym-start)
    (when (string-match janet-ts--slashed-symbol-regexp sym-text)
      (let* ((preslash (match-string 1 sym-text))
             (preslash-end (+ sym-start (length preslash))))
        ;; XXX
        ;; (message "preslash-end: %s" preslash-end)
        ;; (message "preslash: %s" preslash)
        ;; preslash portion
        (treesit-fontify-with-override sym-start preslash-end
                                       ;; XXX: different face?
                                       'font-lock-variable-name-face ; orange
                                       t
                                       start end)))))

;; XXX: for reference, colors that show up when using monokai theme
;;
;; font-lock-comment-face ; dark gray
;; font-lock-comment-delimiter-face ; dark gray
;; font-lock-string-face ; yellow
;; font-lock-doc-face ; dark gray / yellow
;; font-lock-doc-markup-face ; non-existent
;; font-lock-keyword-face ; red
;; font-lock-builtin-face ; red
;; font-lock-function-name-face ; green
;; font-lock-variable-name-face ; orange
;; font-lock-type-face ; blue
;; font-lock-constant-face ; violet
;; font-lock-warning-face ; orange
;; font-lock-negation-char-face ; yellow
;; font-lock-preprocessor-face ; red
;; font-lock-regexp-grouping-backslash ; violet
;; font-lock-regexp-grouping-construct ; yellow
;; font-lock-escape-face ; non-existent
;; font-lock-number-face ; non-existent
;; font-lock-operator-face ; non-existent
;; font-lock-property-face ; non-exitent
;; font-lock-punctuation-face ; non-exitent
;; font-lock-bracket-face ; non-exitent
;; font-lock-delimiter-face ; white?
;; font-lock-misc-punctuation-face ; white?

;; code for trying out queries

;; (let ((query
;;        `(((par_tup_lit "(" (sym_lit) @font-lock-keyword-face)
;;           (:match ,janet-ts--builtin-symbol-regexp
;;                   @font-lock-keyword-face)))))
;;   ;(treesit-query-validate 'janet-simple query)
;;   (treesit-query-string "(def a 1)" query 'janet-simple)
;;   )

;; (let ((query
;;        `(((par_tup_lit :anchor "("
;;                        :anchor (sym_lit) @font-lock-keyword-face
;;                        :anchor (sym_lit) @font-lock-variable-name-face)
;;           (:match ,janet-ts--definition-keyword-regexp
;;                   @font-lock-keyword-face)))))
;;   (treesit-query-validate 'janet-simple query)
;;   ;(treesit-query-string "(def a 1)" query 'janet-simple)
;;   )

;; colors below chosen based on monokai-theme.el internals
(defvar janet-ts--treesit-settings
   ;; see "Font-lock" (starter)
   ;; see font-lock.el for facenames
  (treesit-font-lock-rules
   ;; XXX: actually want red and underlined?
   :feature 'error
   :language 'janet-simple
   '((ERROR) @font-lock-warning-face)

   ;; XXX: what about MISSING?

   ;; yellow
   :feature 'string
   :language 'janet-simple
   '([(str_lit) (long_str_lit) (buf_lit) (long_buf_lit)]
     @font-lock-string-face)

   :feature 'number
   :language 'janet-simple
   '((num_lit) @font-lock-number-face)

   ;; violet
   :feature 'constant
   :language 'janet-simple
   '([(bool_lit) (nil_lit)] @font-lock-constant-face)

   ;; violet
   :feature 'keyword
   :language 'janet-simple
   '((kwd_lit) @font-lock-constant-face)

   :feature 'builtin
   :language 'janet-simple
   :override t
   ;; XXX: will also highlight data literals?
   `(;; buiiltin functions - light green
     ((par_tup_lit :anchor "("
                   :anchor (sym_lit) @font-lock-function-name-face)
      (:match ,janet-ts--builtin-function-regexp
              @font-lock-function-name-face))
     ;; buiiltin macros - red
     ((par_tup_lit :anchor "("
                   :anchor (sym_lit) @font-lock-builtin-face)
      (:match ,janet-ts--builtin-macro-regexp
              @font-lock-builtin-face))
     ;; special forms - light blue
     ((par_tup_lit :anchor "("
                   :anchor (sym_lit) @font-lock-keyword-face)
      (:match ,janet-ts--special-form-regexp
              @font-lock-keyword-face))
     ;; builtin dynamics - orange
     ((sym_lit) @font-lock-variable-name-face
      (:match ,janet-ts--builtin-dynamic-regexp
              @font-lock-variable-name-face))
     ;; builtin value - violet (match true, false, nil)
     ((sym_lit) @font-lock-constant-face
      (:match ,janet-ts--builtin-value-regexp
              @font-lock-constant-face))
     ;; jpm's project.janet constructs - green
     ((par_tup_lit :anchor "("
                   :anchor (sym_lit) @font-lock-function-name-face)
      (:match ,janet-ts--jpm-builtin-value-regexp
              @font-lock-function-name-face)))

   ;; gray
   :feature 'comment
   :language 'janet-simple
   '((comment) @font-lock-comment-face)

   ;; orange
   ;; XXX: may not handle the metadata case (e.g. (def defn :macro ...))?
   :feature 'definition
   :language 'janet-simple
   `(((par_tup_lit :anchor "("
                   ;; XXX: want 'builtin to handle just this part...?
                   :anchor (sym_lit) @font-lock-keyword-face
                   :anchor (sym_lit) @font-lock-variable-name-face)
      (:match ,janet-ts--definition-keyword-regexp
              ;; XXX
              @font-lock-keyword-face)))

   ;; symbols with at least one slash
   ;; XXX: may have a conflict with builtin values in 'builtin above -- check
   :feature 'slashed-symbol
   :language 'janet-simple
   ;;:override t
   `(((sym_lit) @janet-ts--fontify-slashed-symbol))

   )
   "Tree-sitter font-lock settings for `janet-ts-mode'.")

(defun janet-ts--indent-special-p (node)
  "Determine if NODE should be special-case-indented."
  ;; XXX: possible for there to be no child nodes?
  (let* ((head-node (treesit-node-child node 0 'named))
         (head-text (treesit-node-text head-node 'no-prop)))
    ;; XXX
    ;; (message "janet-ts--indent-special-p: %S" head-text)
    (or (member head-text
                ;; XXX: not sure if this list is up-to-date
                ;; XXX: comptime? other things?
                '("case" "comment" "compif" "compwhen" "cond" "coro"
                  "def" "def-" "default" "defdyn" "defer" "defglobal"
                  "defmacro" "defmacro-" "defn" "defn-" "do"
                  "each" "eachk" "eachp"
                  "eachy" ;; XXX: obsolete
                  "edefer" "ev/do-thread" "ev/spawn" "ev/with-deadline"
                  "fn" "for" "forever" "forv"
                  "generate"
                  "if" "if-let" "if-not" "if-with"
                  "label" "let" "loop"
                  "match"
                  "prompt"
                  "repeat"
                  "seq" "short-fn"
                  "tabseq" "try"
                  "unless"
                  "var" "var-" "varfn" "varglobal"
                  "when" "when-let" "when-with" "while"
                  "with" "with-dyns" "with-syms" "with-vars"))
        (string-match (rx bos (or "def" "if-" "when-" "with-"))
                      head-text))))

;; XXX: consider whether treesit-node-index might be used
(defun janet-ts--count-previous-child-nodes (node line-number)
  "Count NODE's named child nodes that occur before the line LINE-NUMBER."
  (let* ((index 0)
         (cur-node (treesit-node-child node index 'named))
         (done nil))
    (while (and cur-node (not done))
      (let ((node-line-number (line-number-at-pos
                               (treesit-node-start cur-node))))
        (if (>= node-line-number line-number)
            (setq done t)
          (setq index (1+ index))
          (setq cur-node (treesit-node-next-sibling cur-node 'named)))))
    index))

(defun janet-ts--anchor-for-special (node line-number)
  "Return anchor for special-case-indented of NODE given LINE-NUMBER."
  (let* ((count (janet-ts--count-previous-child-nodes node line-number))
         (delim-pos (treesit-node-start node)))
    ;; XXX
    ;; (message "janet-ts--anchor-for-special")
    ;; (message "  count: %s" count)
    ;; (message "  delim-pos: %s" delim-pos)
    (if (zerop count)
        (1+ delim-pos)
      (+ 2 delim-pos))))

(defun janet-ts--anchor-for-funcall (node line-number)
  "Return anchor for vanilla funcalls for NODE given LINE-NUMBER."
  (let* ((count (janet-ts--count-previous-child-nodes node line-number))
         (delim-pos (treesit-node-start node)))
    ;; XXX
    ;; (message "janet-ts--anchor-for-funcall")
    ;; (message "  count: %s" count)
    ;; (message "  delim-pos: %s" delim-pos)
    (cond ((= 0 count)
           (1+ delim-pos))
          ((= 1 count)
           (+ 2 delim-pos))
          (t
           (treesit-node-start (treesit-node-child node 1 'named))))))

(defun janet-ts--anchor-for-par-tup-parent (_node parent bol &rest _)
  "Return anchor for NODE assuming PARENT is of type par_tup_lit.

Assumes that PARENT is NODE's parent.  NODE is unused as PARENT is
sufficient.

See `treesit-simple-indent-rules' for NODE, PARENT, and BOL."
  ;; XXX
  ;; (message "janet-ts--anchor-for-par-tup-parent")
  ;; (message "  node: %s" node)
  ;; (message "  parent: %s" parent)
  ;; (message "  bol: %s" bol)
  ;; XXX: excessive?
  (let ((node-type (treesit-node-type parent)))
    (unless (string-match-p node-type "par_tup_lit")
      (error "Expected type par_tup_lit, but got type: %s" node-type)))
  ;;
  (if (zerop (treesit-node-child-count parent 'named)) ; empty par tuple
      (1+ (treesit-node-start parent))
    (let ((line-no (line-number-at-pos bol)))
      (if (janet-ts--indent-special-p parent)
          (janet-ts--anchor-for-special parent line-no)
        (janet-ts--anchor-for-funcall parent line-no)))))

;; see "Indent" (starter)
(defvar janet-ts--indent-rules
  `((janet-simple
     ;; top-level things should start at column 0
     ((parent-is "source")
      parent-bol 0)
     ;; everything below here is not at the top-level (XXX: right?)
     ;;
     ;; parents that are containers
     ((or (parent-is "sqr_tup_lit") (parent-is "struct_lit"))
      parent 1)
     ((or (parent-is "par_arr_lit") (parent-is "sqr_arr_lit")
          (parent-is "tbl_lit"))
      parent 2) ; leading @ adds an extra column
     ;; node should be named at this point as "(" is handled above
     ((parent-is "par_tup_lit")
      janet-ts--anchor-for-par-tup-parent 0)
     ;; XXX: any other cases?
     )))

(defun janet-ts--treesit-imenu-1 (node-list)
  "Create an imenu alist from NODE-LIST.

NODE-LIST is a list of tree-sitter nodes.

Return an imenu alist which can be a list of (NAME . MARKER), where
NAME is the function's name."
  (mapcar (lambda (n)
            (let* ((name
                    ;; XXX: not doing destructuring yet
                    (let ((name-node
                           (treesit-node-child n 1 'named)))
                      ;; XXX: check whether sym_lit?
                      (treesit-node-text name-node 'no-prop)))
                   (marker
                    (set-marker (make-marker)
                                (treesit-node-start n))))
              (cons name marker)))
          node-list))

(defun janet-ts--collect-defish (node)
  "Return par_tup_lit children of NODE that are defish."
  (treesit-filter-child
   node
   (lambda (n)
     (when (equal (treesit-node-type n) "par_tup_lit")
       (let ((head-node (treesit-node-child n 0 'named)))
         (string-match-p "^def"
                         (treesit-node-text head-node 'no-prop)))))
   'named))

(defun janet-ts--collect-varish (node)
  "Return par_tup_lit children of NODE that are varish."
  (treesit-filter-child
   node
   (lambda (n)
     (when (equal (treesit-node-type n) "par_tup_lit")
       (let ((head-node (treesit-node-child n 0 'named)))
         (string-match-p "^var"
                         (treesit-node-text head-node 'no-prop)))))
   'named))

(defun janet-ts--collect-hidden (node)
  "Return par_tup_lit children of NODE that are hidden.

Hidden things are defish/varish things that are direct children of
top-level compif, compwhen, comptime, or upscope forms.

For example, (upscope (defn h [] :a)) leads to a semantically
top-level definition of a function h, even though the defn is
not syntactically top-level."
  ;; XXX: likely a better way to do this?
  (let ((children
         (treesit-filter-child
          node
          (lambda (n)
            (when (equal (treesit-node-type n) "par_tup_lit")
              (let ((head-node (treesit-node-child n 0 'named)))
                (string-match-p "^\\(comp\\(if\\|when\\|time\\)\\|upscope\\)"
                                (treesit-node-text head-node 'no-prop)))))
          'named)))
    (flatten-list
     (mapcar (lambda (n)
               (append (janet-ts--collect-defish n)
                       (janet-ts--collect-varish n)))
             children))))

(defun janet-ts--imenu-treesit-create-index ()
  "Return imenu alist for the current buffer."
  (let* ((node (treesit-buffer-root-node))
         ;;
         (def-par-tup-list (janet-ts--collect-defish node))
         (var-par-tup-list (janet-ts--collect-varish node))
         (comp*-par-tup-list (janet-ts--collect-hidden node))
         ;; XXX: pp-emacs-lisp-code or ppp (see github) for viewing?
         (defish-index (janet-ts--treesit-imenu-1 def-par-tup-list))
         (varish-index (janet-ts--treesit-imenu-1 var-par-tup-list))
         (hidden-index (janet-ts--treesit-imenu-1 comp*-par-tup-list)))
    (append (when defish-index `(("defish" . ,defish-index)))
            (when varish-index `(("varish" . ,varish-index)))
            (when hidden-index `(("hidden" . ,hidden-index))))))

;; XXX: other things to include?  e.g. fn?
(defconst janet-ts--defun-regexp
  (rx bos
      (or (seq "def"
               (or eos "-" "dyn" "global" "macro" "n"))
          (seq "var"
               (or eos "-" "fn" "global")))))

(defun janet-ts-mode-find-tag-function ()
  "Implementation of `find-tag-default-function' for janet-ts-mode."
  (treesit-node-text (treesit-node-at (point)) 'no-property))

(defvar janet-ts--syn-prop-query
  (when (treesit-available-p)
    (treesit-query-compile 'janet-simple
                           ;; XXX: could merge the two?
                           '(((long_str_lit) @long_str_lit)
                             ((long_buf_lit) @long_buf_lit))
                           ;; not having the following causes failure in a case
                           'eager)))

(defun janet-ts--syntax-propertize (start end)
  "Improve handling of long-strings and long-buffers.

START and END are as described in docs for `syntax-propertize-function'."
  (let ((captures (treesit-query-capture 'janet-simple
                                         janet-ts--syn-prop-query start end)))
    (pcase-dolist (`(,name . ,node) captures)
      (pcase-exhaustive name
        ((or 'long_str_lit 'long_buf_lit)
         (let* ((n-start (treesit-node-start node))
                (n-end (treesit-node-end node))
                ;; bt1 == first backtick
                (bt1-start (cond
                            ((eq name 'long_str_lit)
                             n-start)
                            ;;
                            ((eq name 'long_buf_lit)
                             (1+ n-start))
                            ;;
                            (t
                             (error "Unexpected node type: %S" name)))))
           (put-text-property bt1-start (1+ bt1-start)
                              'syntax-table (string-to-syntax "|"))
           (put-text-property (1- n-end) n-end
                              'syntax-table (string-to-syntax "|"))
           ;; tweaking syntax class of some chars for better behavior
           (let ((pos (1+ bt1-start))
                 (stop (1- n-end)))
             (while (< pos stop)
               (when (not (memq (char-after pos)
                                ;; XXX: might need to tweak this further
                                (list ?\s ?\n ?\r ?\t ?\f ?\v
                                      ?\( ?\) ?\[ ?\] ?{ ?})))
                 (put-text-property pos (1+ pos)
                                    'syntax-table
                                    (string-to-syntax "w")))
               (setq pos (1+ pos))))))))))

;; see `(elisp) Tree-sitter major modes'
;;;###autoload
(define-derived-mode janet-ts-mode prog-mode "Janet"
  "Major mode for editing Janet, powered by tree-sitter."
  :syntax-table janet-ts--syntax-table
  ;;
  (unless (treesit-ready-p 'janet-simple)
    (error "Tree-sitter for Janet isn't ready"))
  ;;
  ;; commenting
  ;;
  ;; this gets comment-region to work
  (setq-local comment-start "#")
  ;;
  (treesit-parser-create 'janet-simple)
  ;;
  ;; highlighting
  ;;
  ;; see `(elisp) Parser-based Font Lock' and "Font-lock" (starter)
  ;;
  (setq-local treesit-font-lock-settings janet-ts--treesit-settings)
  (setq-local treesit-font-lock-feature-list
              '((comment error string number)
                (keyword constant definition)
                (builtin)
                (slashed-symbol)
                ))
  ;; setting treesit-font-lock-level adjusts how much is enabled
  (setq-local treesit-font-lock-level 3)
  ;;(setq-local treesit-font-lock-level 4)
  ;;
  ;; indentation
  ;;
  ;; see `(elisp) Parser-based Indentation' and "Indent" (starter)
  ;;
  (setq-local treesit-simple-indent-rules janet-ts--indent-rules)
  ;;
  ;; imenu
  ;;
  (setq-local imenu-create-index-function
              #'janet-ts--imenu-treesit-create-index)
  ;;
  ;; navigation -- see "Navigation" (starter)
  ;;
  ;; XXX: to test, use treesit-beginning-of-defun and
  ;;      treesit-end-of-defun
  ;;
  ;; XXX: weird behavior for treesit-end-of-defun near things like
  ;;      compwhen, do, etc.
  (setq-local treesit-defun-prefer-top-level nil
              treesit-defun-tactic 'nested
              ;; Sometimes not all nodes matched by the regexp are
              ;; valid defuns.  In that case, set this variable to a
              ;; cons cell of the form (REGEXP . FILTER), where FILTER
              ;; is a function that takes a node (the matched node)
              ;; and returns t if node is valid, or nil for invalid
              ;; node.
              treesit-defun-type-regexp
              (cons (rx "par_tup_lit")
                    (lambda (n)
                      (let* ((head-node
                              (treesit-node-child n 0 'named))
                             (head-text
                              (treesit-node-text head-node 'no-prop)))
                        ;; XXX: string-match-p better?
                        (string-match janet-ts--defun-regexp head-text))))
              treesit-defun-skipper
              (lambda ()
                (skip-chars-forward " \t")))
  ;;
  ;; which-func -- see "Which-fun" (starter)
  ;;
  ;; XXX: this isn't perfect, but it does work for some common cases.
  ;;      it will even see defish and varish things that live as
  ;;      direct children inside top-level compif, comptime, compwhen,
  ;;      and upscope constructs.  though there is at least one issue.
  ;;      for example, for (upscope (defn a [] 1)), if point is
  ;;      between upscope and the open paren before defn, the function
  ;;      reported by which-func will be that which precedes upscope.
  ;;      or something like that :)
  (setq-local which-func-functions nil)
  ;;
  (treesit-major-mode-setup)
  ;;
  ;; for buffer-local faces -- vis `face-remapping-alist' docs
  ;; e.g. want special forms to turn up light blue in monokai
  ;;(set (make-local-variable 'face-remapping-alist)
  ;;     (copy-tree '((font-lock-keyword-face font-lock-type-face))))
  ;;
  ;; code navigation via tags
  ;;
  (setq-local find-tag-default-function 'janet-ts-mode-find-tag-function)
  ;;
  ;; syntax property things for handling long strings
  ;;
  (setq-local syntax-propertize-function #'janet-ts--syntax-propertize)
  ;;
  )

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.janet\\'" . janet-ts-mode))
;; XXX: haven't really tested
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.jdn\\'" . janet-ts-mode))

;; node info in mode line
(add-hook 'janet-ts-mode-hook 'treesit-inspect-mode)

(provide 'janet-ts-mode)
;;; janet-ts-mode.el ends here
