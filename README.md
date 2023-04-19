# janet-ts-mode

A tree-sitter-based Emacs major mode for the Janet programming language

![Highlighting Sample](janet-ts-mode-highlighting.png?raw=true "Highligting Sample")

## Status

Proof-of-concept, though some things are more-or-less usable:

* Highlighting (fair bit working)
* Indentation (fair bit working)
* Imenu (somewhat functional)
* Navigation (somewhat functional)
* Which-Func (somewhat functional)

## Dependencies

* [tree-sitter](https://github.com/tree-sitter/tree-sitter)
* [Emacs with tree-sitter
  support](https://github.com/emacs-mirror/emacs) (master branch or >=
  emacs-29)
* [tree-sitter-module](https://github.com/casouri/tree-sitter-module)
  for getting a dynamic library for tree-sitter-janet-simple
  (`./build.sh janet-simple`)

## Setup

* [Fulfill dependencies and teach Emacs about tree-sitter grammar
  dynamic library location (Debian / Ubuntu
  version)](https://gist.github.com/sogaiu/a13512e146e8f5c0e94d1804838558ee)
  -- this should become easier once Emacs-tree-sitter integration
  ships.  Alternatively, see [this
  post](https://blog.markhepburn.com/posts/experimenting-with-the-built-in-treesitter-support-in-emacs/)
  which gives instructions that are a bit less distribution-specific
  (though there is a trade-off regarding setup of things so that Emacs
  knows where to find the tree-sitter library).

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
  * Observe some syntax highligthing :)
  * `M-x imenu`
  * `M-x which-function-mode`
  * `M-x treesit-end-of-defun` - might be a bit weird due to
    [this](https://github.com/tree-sitter/tree-sitter-bash/issues/139)
  * `M-x treesit-beginning-of-defun`

* Developer-ish things:
  * Observe the node information in the mode line - couresty of
    `treesit-inspect-mode`
  * `M-x treesit-explore-mode`
  * `M-: (treesit-query-validate ...)` and `M-: (treesit-query-string
    ...)` -- see comments in `.el` file

## Possible Future Activities

* Menu
* Folding
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
* thechampagne - autoloads
* ubolonton and elisp-tree-sitter contributors
* yrns - PR 11 to janet-mode

