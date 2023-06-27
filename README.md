# janet-ts-mode

A tree-sitter-based Emacs major mode for the Janet programming language

![Highlighting Sample](janet-ts-mode-highlighting.png?raw=true "Highlighting Sample")

## Status

Author uses as primary means of editing Janet code.

Currently has more-or-less usable:

* Highlighting (fair bit working)
* Indentation (fair bit working)
* Imenu (somewhat functional)
* Navigation (somewhat functional)
* Which-Func (somewhat functional)

See the "Things to try" section below for additional details.

## Dependencies

* [tree-sitter](https://github.com/tree-sitter/tree-sitter)
* [Emacs with tree-sitter
  support](https://github.com/emacs-mirror/emacs) (master branch or >=
  emacs-29)
* [tree-sitter-module](https://github.com/casouri/tree-sitter-module)
  for getting a dynamic library for tree-sitter-janet-simple
  (`./build.sh janet-simple`)

## Setup

* Fulfill dependencies and teach Emacs about tree-sitter grammar
  dynamic library location
  * [Debian / Ubuntu](https://gist.github.com/sogaiu/a13512e146e8f5c0e94d1804838558ee)
  -- this should become easier once Emacs-tree-sitter integration
  ships.
  * [Void](https://gist.github.com/sogaiu/088e55664970dde57a30b725bb7b4707) -- the easiest way I've tried so far :)
  * [These
  steps](https://blog.markhepburn.com/posts/experimenting-with-the-built-in-treesitter-support-in-emacs/)
  give instructions that are a bit less distribution-specific (though
  there is a trade-off regarding setup of things so that Emacs knows
  where to find the tree-sitter library).
  *

* janet-ts-mode prep

    * Manual
        * `cd ~/src # or some place appropriate`
        * `git clone https://github.com/sogaiu/janet-ts-mode`
        * edit .emacs-equivalent so that:
            * `load-path` knows about janet-ts-mode
            * `(require 'janet-ts-mode)` is included
    * Straight - add something like the following to .emacs-equivalent:
        ```
        (straight-use-package
         '(janet-ts-mode :host github
                         :repo "sogaiu/janet-ts-mode"
                         :files ("*.el")))

        (use-package janet-ts-mode
          :straight t)
        ```

## Things to try

* Open a `.janet` file, then:
  * `M-x janet-ts-mode` to enable the major mode
  * Try indenting some code
  * Observe some syntax highlighting :)
  * `M-x imenu`
  * `M-x which-function-mode`
  * `M-x treesit-end-of-defun` - might be a bit weird due to
    [this](https://github.com/tree-sitter/tree-sitter-bash/issues/139)
  * `M-x treesit-beginning-of-defun`

* Developer-ish things:
  * Observe the node information in the mode line - courtesy of
    `treesit-inspect-mode`
  * `M-x treesit-explore-mode`
  * `M-: (treesit-query-validate ...)` and `M-: (treesit-query-string
    ...)` -- see comments in `.el` file

* There's a file named `janet-ts-experiment.el` that contains a
  variety of convenience commands.  As the file name suggests, the
  content is experimental.  Not sure what if anything I'll keep from
  it, but currently it has:

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
  Janet-TS menu.

## Possible Future Activities

* Filling out, refining, modifying existing things mentioned above :)

## Credits

* ahungry - theme and other discussions
* AlbertoGP - PR 12 to janet-mode
* bakpakin and Janet contributors
* bbatsov - clojure-ts-mode discussions
* casouri and other Emacs contributors - Emacs tree-sitter integration
* dannyfreeman - clojure-ts-mode and discussions
* dgutov - ruby-ts--syntax-propertize work
* Fanael - rainbow-delimiters and syntax table discussion
* maxbrunsfeld and tree-sitter contributors - tree-sitter
* ml-2 - suggestions and discussions
* thechampagne - autoloads
* ubolonton and elisp-tree-sitter contributors
* yrns - PR 11 to janet-mode

