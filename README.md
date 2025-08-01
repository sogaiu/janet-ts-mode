# janet-ts-mode

A tree-sitter-based Emacs major mode for the Janet programming language

![Highlighting Sample](janet-ts-mode-highlighting.png?raw=true "Highlighting Sample")

## Status

Author uses as primary means of editing Janet code.

Currently has more-or-less usable:

* Highlighting
* Indentation
* Imenu
* Navigation
* Which-Func

See the "Things to try" section below for additional details.

## Quick Taste

For a recent Emacs (>= 29.1) with the integrated tree-sitter support,
[janet-emacs-trial-kit](https://github.com/sogaiu/janet-emacs-trial-kit)
may be a way of trying out `janet-ts-mode` along with some other
janet-supporting packages without having to change any existing
settings or manually download anything additional.

## Dependencies

* [Emacs with tree-sitter
  support](https://github.com/emacs-mirror/emacs) (master branch or >=
  emacs-29)

## Setup

* If Emacs >= 29.1 with tree-sitter support is in place [1], something
  like the following in your .emacs equivalent should arrange for the
  tree-sitter-janet-simple grammar to be available:

    ```elisp
    (setq treesit-language-source-alist
          (if (eq 'windows-nt system-type)
              '((janet-simple
                 . ("https://github.com/sogaiu/tree-sitter-janet-simple"
                    nil nil "gcc.exe")))
            '((janet-simple
               . ("https://github.com/sogaiu/tree-sitter-janet-simple")))))

    (when (not (treesit-language-available-p 'janet-simple))
      (treesit-install-language-grammar 'janet-simple))
    ```

* janet-ts-mode prep

    * Manual
        * `cd ~/src # or some place appropriate`
        * `git clone https://github.com/sogaiu/janet-ts-mode`
        * edit your .emacs equivalent so that:
            * `load-path` knows about janet-ts-mode
            * `(require 'janet-ts-mode)` is included

    * use-package

        ```elisp
        (use-package janet-ts-mode
          :vc (:url "https://github.com/sogaiu/janet-ts-mode"
               :rev :newest))
        ```

    * Elpaca - add something like the following to your .emacs
      equivalent:

        ```elisp
        (use-package janet-ts-mode
          :elpaca (:host github
                   :repo "sogaiu/janet-ts-mode"
                   :files ("*.el")))
        ```

    * Straight.el - add something like the following to your .emacs
      equivalent:

        ```elisp
        (straight-use-package
         '(janet-ts-mode :host github
                         :repo "sogaiu/janet-ts-mode"
                         :files ("*.el")))

        (use-package janet-ts-mode
          :straight t)
        ```

## Things to try

First, open a `.janet` file and check that the current major mode is
`janet-ts-mode` (e.g. via `C-h m`).  Try `M-x janet-ts-mode` if it
isn't.

### Basics

* Syntax highlighting - if things are working appropriately, non-empty
  buffer content should be appropriately highlighted.

* Indentation - type in some code and press the `Tab` key to indent
  the current line, or select a region and press the `Tab` key to
  indent the region.

* Imenu - to end up at a particular top-level definition, `M-x imenu`,
  and follow the prompts.  Alternatively, look for a menu named
  `Index` and interact with that appropriately :)

* Navigation - use `M-x treesit-end-of-defun` (`C-M-e`) to navigate to
  the end of the current `defun` (kind of tricky to explain, but
  something like "top-level" form for the case of a lispy language in
  Emacs), or `M-x treesit-beginning-of-defun` (`C-M-a`) to navigate to
  the beginning of the current `defun`, or if not within a `defun`,
  conveniently navigate through the buffer by repeatedly invoking `M-x
  treesit-end-of-defun` to go in the forward direction (toward the end
  of the buffer), or `M-x treesit-beginning-of-defun` to go in the
  reverse direction (toward the beginning of the buffer).

* Which-Func - when within a top-level definition, to see a guessed
  name for the definition, `M-x which-function-mode`, and observe the
  mode line.  If things went well, probably toward the right of the
  mode line there should be some text like: `[nice-function-name]` if
  point is within a definition with name `nice-function-name`.

### Developer-ish Things

* Tree-sitter's view of the source - `M-x treesit-explore-mode` and
  choose `janet-simple` to open a second buffer (named `*tree-sitter
  explorer for ...*`) that shows a view of what the tree-sitter
  grammar thinks the code in the buffer parses as.  Likely there will
  be a fair number of instances of things like `sym_lit`,
  `par_tup_lit`, `num_lit`, etc.  These are node names associated with
  the
  [tree-sitter-janet-simple](https://github.com/sogaiu/tree-sitter-janet-simple)
  grammar.  Clicking on node names in the newly opened buffer should
  cause point in the corresponding buffer with Janet source code in it
  to move accordingly.

* Tree-sitter queries - `M-: (treesit-query-validate ...)` and
  `M-: (treesit-query-string ...)` ... it's kind of complicated...please
  see comments in `janet-ts-mode.el` (^^;

### Experimental Things

There's a file named `janet-ts-experiment.el` that contains a variety
of convenience commands.  As the file name suggests, the content is
experimental.  Not sure what if anything I'll keep from it, but
currently it has:

* Selection
  * Select something relevant around point
  * Expand current selection

* Formatting Helpers
  * Split across lines, content of selection at opening parens
  * Format pairs within selection to be on own lines

* Wrapping
  * Wrap something at point in `tracev` call
  * Unwrap a `tracev` call that contains point

* Comment and Long-String Folding
  * Fold aforementioned forms so they don't take much space
  * Unfold folded forms so the content can be seen
  * Toggle the folding of certain forms

* Delimiters
  * Cycle delimiters: `(...)` -> `[...]` -> `{...}` and back to parens
  * Move right delimiter at point over the next thing to the right -
    if you're not a structural editor user and have found
    auto-balanced delimiters (particularly parens) to get in your
    way, this function might be appealing.

If the file is `require`d, it should add various things to the
`Janet-TS` menu.

There's another file named `janet-ts-helpers.el` that provides limited
integration with a number of Janet-related command line utilities:

* [jdoc](https://github.com/sogaiu/jdoc)
* [jref](https://github.com/sogaiu/janet-ref)
* [pdoc](https://github.com/sogaiu/janet-pegdoc)

Again, if the file is `require`d, it should add various things to the
`Janet-TS` menu.

## Credits

* ahungry - theme and other discussions
* AlbertoGP - PR 12 to janet-mode
* alternateved - report and suggestion
* amano.kenji - testing and installation instruction suggestions
* bakpakin and Janet contributors
* bbatsov - clojure-ts-mode discussions
* casouri and other Emacs contributors - Emacs tree-sitter integration
* CosmicToast - use-package installation info
* dannyfreeman - clojure-ts-mode and discussions
* dgutov - ruby-ts--syntax-propertize work
* Fanael - rainbow-delimiters and syntax table discussion
* maxbrunsfeld and tree-sitter contributors - tree-sitter
* ml-2 - suggestions and discussions
* thechampagne - autoloads
* ubolonton and elisp-tree-sitter contributors
* yrns - PR 11 to janet-mode

## Footnotes

[1] If building Emacs >= 29.1 from source (typically to get it with tree-sitter support), links below might be of interest:

* [Debian / Ubuntu](https://gist.github.com/sogaiu/a13512e146e8f5c0e94d1804838558ee) -- this should become easier once Emacs-tree-sitter integration ships.
* [Void](https://gist.github.com/sogaiu/088e55664970dde57a30b725bb7b4707) -- the easiest way I've tried so far :)
* [These steps](https://blog.markhepburn.com/posts/experimenting-with-the-built-in-treesitter-support-in-emacs/) give instructions that are a bit less distribution-specific (though there is a trade-off regarding setup of things so that Emacs knows where to find the tree-sitter library).
* [How to Get Started with Tree-Sitter](https://www.masteringemacs.org/article/how-to-get-started-tree-sitter) might be worth looking at, though I haven't examined the details.

