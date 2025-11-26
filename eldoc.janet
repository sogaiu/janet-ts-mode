#! /usr/bin/env janet

# XXX: untested on windows

(defn specials-sig
  [sym-name]
  (case sym-name
    "def" "name meta... value"
    "var" "name meta... value"
    "fn" "name? args body..."
    "do" "body..."
    "quote" "x"
    "if" "condition when-true when-false?"
    "splice" "x"
    "while" "condition body..."
    "break" "value?"
    "set" "l-value r-value"
    "quasiquote" "x"
    "unquote" "x"
    "upscope" "& body"
    nil))

(defn builtin-sig
  "Searches for signature of `sym`."
  [sym]
  (def doc-str
    (get-in root-env [sym :doc]))
  (when (not doc-str)
    (break false))
  #
  (def parts (string/split "\n\n" doc-str))
  (when (not parts)
    (break false))
  #
  (def sig (string/trim (get parts 0)))
  (when (not (string/find " " sig))
    (break ""))
  # XXX: not sure if parens don't occur inside sig...
  (def m
    (peg/match ~(sequence "("
                          (thru " ")
                          (capture (to ")"))
                          ")"
                          -1) sig))
  #(eprintf "peg/match: %n" m)
  (when (not m)
    (break false))
  #
  (get m 0))

(defn main
  [_ & args]
  (def sym-name (get args 0))
  (when (not sym-name)
    (print "")
    (os/exit 0))
  #
  (def sp-result (specials-sig sym-name))
  (when sp-result
    (printf "(%s)" sp-result)
    (os/exit 0))
  #
  (def sym (symbol sym-name))
  (def result (builtin-sig sym))
  (when result
    (printf "(%s)" result)
    (os/exit 0))
  #
  # XXX: support project identifiers?
  #
  # default
  (print ""))

