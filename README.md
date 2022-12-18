# janet-ts-mode

A tree-sitter-based Emacs major mode for the Janet programming language

## Status

Proof-of-concept, though some things are more-or-less usable:

* Highlighting (fair bit working)
* Indentation (fair bit working)
* Imenu (somewhat functional)
* Navigation (somewhat functional)
* Which-Func (somewhat functional)

## Dependencies

* [tree-sitter](https://github.com/tree-sitter/tree-sitter)
* [Emacs with tree-sitter support](https://github.com/emacs-mirror/emacs) (master branch or >= emacs-29)
* [tree-sitter-module](https://github.com/casouri/tree-sitter-module) + patch to build janet-simple grammar

## Setup

* [Get and put dependencies in place (Ubuntu version)](https://gist.github.com/sogaiu/a13512e146e8f5c0e94d1804838558ee) -- this should become easier once Emacs-tree-sitter integration ships.
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
  * `M-x treesit-end-of-defun` - might be a bit weird due to [this](https://github.com/tree-sitter/tree-sitter-bash/issues/139)
  * `M-x treesit-beginning-of-defun`

* Developer-ish things:
  * Observe the node information in the mode line - couresty of `treesit-inspect-mode`
  * `M-x treesit-explore-mode`
  * `M-: (treesit-query-validate ...)` and `M-: (treesit-query-string ...)` -- see comments in `.el` file

## Possible Future Activities

* Menu
* Syntax Table Stuff (not sure if it's worth it)
* Folding
* Filling out, refining, modifying existing things mentioned above :)

## Credits

* ahungry - theme and other discussions
* bakpakin and Janet contributors
* casouri and other Emacs contributors - Emacs tree-sitter integration
* dannyfreeman - clojure-ts-mode and discussions
* maxbrunsfeld and tree-sitter contributors - tree-sitter
* ubolonton and elisp-tree-sitter contributors
