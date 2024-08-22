;;; janet-ts-experiment --- Experimental features

;;; Commentary:

;;; Code:

(require 'janet-ts-mode)
(require 'treesit)

(defun janet-ts-cycle-delimiters ()
  "Cycle delimiters of relevant container with respect to point."
  (interactive)
  (let* ((curr-node (treesit-node-at (point)))
         (parent (treesit-node-parent curr-node))
         (p-type (treesit-node-type parent)))
    (when (and p-type
               (member p-type (list "par_tup_lit" "sqr_tup_lit" "struct_lit"
                                    "par_arr_lit" "sqr_arr_lit" "tbl_lit")))
      (let ((dl (+ (treesit-node-start parent)
                   (if (member p-type
                               (list "par_arr_lit" "sqr_arr_lit" "tbl_lit"))
                       1 ; skip leading @
                     0)))
            (dr (treesit-node-end parent))
            (l-delim-alist '((?\( . ?\[)
                             (?\[ . ?\{)
                             (?\{ . ?\()))
            (r-delim-alist '((?\) . ?\])
                             (?\] . ?\})
                             (?\} . ?\))))
            (l-delim nil)
            (r-delim nil))
        (save-excursion
          (goto-char dl)
          (setq l-delim (char-after (point)))
          (delete-char 1)
          (insert (alist-get l-delim l-delim-alist))
          (goto-char (1- dr))
          (setq r-delim (char-after (point)))
          (delete-char 1)
          (insert (alist-get r-delim r-delim-alist)))))))

(defun janet-ts--node-is-named (node)
  "Determine if NODE is named."
  (member (treesit-node-type node)
          '("comment"
            "nil_lit" "bool_lit" "num_lit" "kwd_lit" "sym_lit"
            "str_lit" "long_str_lit"
            "buf_lit" "long_buf_lit")))

(defun janet-ts--bounds-calculate ()
  "Calculate bounds for Janet thing at point."
  (when-let ((curr-node (treesit-node-at (point))))
    (if (janet-ts--node-is-named curr-node)
      (list (treesit-node-start curr-node)
            (treesit-node-end curr-node))
      (when-let ((parent-node (treesit-node-parent curr-node)))
        (list (treesit-node-start parent-node)
              (treesit-node-end parent-node))))))

(defun janet-ts-swap-with-next ()
  "Swap current thing with next thing."
  (interactive)
  (when-let* ((result (janet-ts--bounds-calculate))
              (beg (nth 0 result))
              (end (nth 1 result))
              (point-node (treesit-node-at (point)))
              (curr-node (if (janet-ts--node-is-named point-node)
                             point-node
                           (treesit-node-parent point-node))))
    (when-let* ((next-sibling (treesit-node-next-sibling curr-node :named))
                (ns-beg (treesit-node-start next-sibling))
                (ns-end (treesit-node-end next-sibling)))
      (save-excursion
        ;; XXX: did not understand yank well enough to use it here
        (let ((temp-point nil)
              (ws nil)
              (ns-text nil))
          (goto-char end)
          (skip-chars-forward " \t\n")
          (setq temp-point (point))
          (setq ws (buffer-substring end temp-point))
          (setq ns-text (buffer-substring ns-beg ns-end))
          (kill-region ns-beg ns-end)
          (kill-region end temp-point)
          (goto-char beg)
          (insert ns-text ws))))))

(defvar-local janet-ts-comment-block-folds nil)

(defun janet-ts-unfold-comment-blocks ()
  "Unfold top level comment blocks."
  (interactive)
  (seq-doseq (ov janet-ts-comment-block-folds)
    (delete-overlay ov))
  (setq janet-ts-comment-block-folds nil))

(defun janet-ts-fold-comment-blocks ()
  "Fold top level comment blocks."
  (interactive)
  (let* ((root (treesit-buffer-root-node))
         (index 0)
         (cur-node (treesit-node-child root index :named)))
    (janet-ts-unfold-comment-blocks)
    (setq janet-ts-comment-block-folds ())
    (while cur-node
      (let ((node-type (treesit-node-type cur-node)))
        (when (and (string= "par_tup_lit" node-type)
                   (< 0 (treesit-node-child-count cur-node :named)))
          (let ((head-node (treesit-node-child cur-node 0 :named)))
            (when (and (string= "sym_lit"
                           (treesit-node-type head-node))
                       (string= "comment"
                            (treesit-node-text head-node)))
              (let ((ov (make-overlay (treesit-node-end head-node)
                                      (treesit-node-end cur-node))))
                (overlay-put ov 'invisible t)
                (overlay-put ov 'after-string "...)")
                (setq janet-ts-comment-block-folds
                      (cons ov janet-ts-comment-block-folds)))))))
      (setq index (1+ index))
      (setq cur-node (treesit-node-child root index :named)))))

(defun janet-ts-toggle-comment-blocks ()
  "Toggle folding of comment blocks."
  (interactive)
  (if janet-ts-comment-block-folds
      (janet-ts-unfold-comment-blocks)
    (janet-ts-fold-comment-blocks)))

(defvar-local janet-ts-long-string-folds nil)

(defun janet-ts-unfold-long-strings ()
  "Unfold long strings."
  (interactive)
  (seq-doseq (ov janet-ts-long-string-folds)
    (delete-overlay ov))
  (setq janet-ts-long-string-folds nil))

(defun janet-ts-fold-long-strings ()
  "Fold long strings."
  (interactive)
  (let* ((root (treesit-buffer-root-node))
         (index 0)
         (cur-node (treesit-node-child root index :named)))
    (janet-ts-unfold-long-strings)
    (setq janet-ts-long-string-folds ())
    (while cur-node
      (let ((node-type (treesit-node-type cur-node)))
        (when (and (string= "par_tup_lit" node-type)
                   (< 2 (treesit-node-child-count cur-node :named)))
          (let ((third-node (treesit-node-child cur-node 2 :named)))
            (when (string= "long_str_lit" (treesit-node-type third-node))
              (let ((ov (make-overlay (1+ (treesit-node-start third-node))
                                      (treesit-node-end third-node))))
                (overlay-put ov 'invisible t)
                (overlay-put ov 'after-string "...`")
                (setq janet-ts-long-string-folds
                      (cons ov janet-ts-long-string-folds)))))))
      (setq index (1+ index))
      (setq cur-node (treesit-node-child root index :named)))))

(defun janet-ts-toggle-long-strings ()
  "Toggle folding of long strings."
  (interactive)
  (if janet-ts-long-string-folds
      (janet-ts-unfold-long-strings)
    (janet-ts-fold-long-strings)))

;; XXX: undoing doesn't restore cursor position...
(defun janet-ts-move-left-delim-right ()
  "Try to move left delimiter at point over the next thing to the right."
  (interactive)
  (let* ((start-spot (point))
         (curr-node (treesit-node-at start-spot))
         (node-text (treesit-node-text curr-node :no-property))
         (parent-node (treesit-node-parent curr-node))
         (p-type (treesit-node-type parent-node)))
    (when (and p-type
               (member node-text (list "(" "[" "{"))
               (member p-type
                       (list "par_tup_lit" "sqr_tup_lit" "struct_lit")))
      (when-let* ((right-node (treesit-node-next-sibling curr-node))
                  (end-of-rn (treesit-node-end right-node)))
        (save-mark-and-excursion
          (goto-char end-of-rn)
          (skip-chars-forward " \t")
          (insert node-text)
          (goto-char start-spot)
          (delete-char 1))))))

;;;###autoload
(defun janet-ts-move-right-delim-right ()
  "Try to move right delimiter at point over the next thing to the right."
  (interactive)
  (let* ((start-spot (point))
         (curr-node (treesit-node-at start-spot))
         (node-text (treesit-node-text curr-node :no-property))
         (parent-node (treesit-node-parent curr-node))
         (p-type (treesit-node-type parent-node)))
    (when (and p-type
               (member node-text (list ")" "]" "}"))
               (member p-type
                       (list "par_tup_lit" "sqr_tup_lit" "struct_lit")))
      (when-let* ((right-node (treesit-node-next-sibling parent-node))
                  (end-of-rn (treesit-node-end right-node)))
        (save-excursion
          (goto-char end-of-rn)
          (insert node-text)
          (goto-char start-spot)
          (delete-char 1)
          (insert " "))))))

(defun janet-ts-expand-selection ()
  "Expand selection based on parent node boundaries."
  (interactive)
  (if (not mark-active)
      (when-let* ((result (janet-ts--bounds-calculate))
                  (beg (nth 0 result))
                  (end (nth 1 result)))
        (set-mark beg)
        (goto-char end)
        (activate-mark))
    (let* ((beg (min (point) (mark)))
           (beg-node (treesit-node-at beg))
           (maybe-parent-node (treesit-node-parent beg-node))
           (parent-node (if (janet-ts--node-is-named beg-node)
                            maybe-parent-node
                          (treesit-node-parent maybe-parent-node))))
      (when parent-node
        (let* ((p-beg (treesit-node-start parent-node))
               (p-end (treesit-node-end parent-node)))
          (set-mark p-beg)
          (goto-char p-end)
          (activate-mark))))))

(defun janet-ts-select ()
  "Select appropriately around point."
  (interactive)
  (janet-ts-expand-selection))

(defun janet-ts-format-selection-as-code ()
  "Format selection as code."
  (interactive)
  ;; https://github.com/sogaiu/janet-ref
  (shell-command-on-region (point) (mark) "jref -f" nil :replace))

(defun janet-ts-format-selection-split-at-parens ()
  "Put newlines before parens in region and indent."
  (interactive)
  ;; 1. replace ( by nl (
  ;; 2. indent region
  (let* ((start (min (point) (mark)))
         (end (max (point) (mark)))
         (end-marker (make-marker)))
    (set-marker end-marker end)
    (save-excursion
      (goto-char start)
      (while (search-forward "(" end-marker t)
        (when (string= "("
                       (treesit-node-type (treesit-node-at (1- (point)))))
          (goto-char (1- (point)))
          (insert "\n"))
        (goto-char (1+ (point)))))
    (indent-region start end)))

(defun janet-ts-format-selection-pairs ()
  "Put newlines after all but last top pair in region."
  (interactive)
  ;; 1. find outermost container in region
  ;; 2. put a newline after each "top" pair in found container (except last)
  ;; 3. indent region
  (let* ((start (min (point) (mark)))
         (end (max (point) (mark)))
         (end-marker (make-marker)))
    (set-marker end-marker end)
    (save-excursion
      (goto-char start)
      (let* ((curr-node (treesit-node-at (point)))
             (parent-node (treesit-node-parent curr-node))
             (n-children (treesit-node-child-count parent-node :named))
             (total (round (/ (- n-children 2) 2.0)))
             (counter 0)
             (target-child nil))
        (while (< counter total)
          ;; seems that node info gets out of date
          (setq curr-node (treesit-node-at start))
          (setq parent-node (treesit-node-parent curr-node))
          (setq target-child
                (treesit-node-child parent-node (1+ (* counter 2)) :named))
          (goto-char (treesit-node-end target-child))
          (insert "\n")
          (setq counter (1+ counter)))))
      (indent-region start end)))

(defun janet-ts-format-selection-as-data ()
  "Format selection as data."
  (interactive)
  ;; https://github.com/sogaiu/janet-ref
  (shell-command-on-region (point) (mark) "jref -p" nil :replace))

(defun janet-ts--wrap-with (name-ish)
  "Wrap with call to NAME-ISH.

If NAME-ISH is an empty string or ends with a space or newline,
inserts NAME-ISH as-is.  Otherwise, inserts a space after
NAME-ISH."
  (when-let ((result (janet-ts--bounds-calculate)))
    (let ((start-pos (nth 0 result))
          (end-pos (nth 1 result))
          (start-part (if (or (string-empty-p name-ish)
                              (string-suffix-p " " name-ish)
                              (string-suffix-p "\n" name-ish))
                          name-ish
                        (concat name-ish " "))))
      ;; replace
      (kill-region start-pos end-pos)
      (insert (concat "(" start-part))
      (yank)
      (insert ")")
      ;; tidy
      (indent-region start-pos (point)))))

(defun janet-ts-wrap-in-tracev-call ()
  "Wrap suitably chosen form in tracev call."
  (interactive)
  (janet-ts--wrap-with "tracev"))

(defun janet-ts--wrap-calculate (name)
  "Calculate form bounds for tuple with head symbol NAME containing point.

NAME can be nil, in which case, no climbing happens beyond the
innermost containing call form."
  (save-excursion
    (let* ((curr-node (treesit-node-at (point)))
           (call-tuple-node
            (treesit-parent-until
             curr-node
             (lambda (parent-node)
               (when-let* ((parent-type (treesit-node-type parent-node))
                           (head-node
                            (treesit-node-child parent-node 0 :named))
                           (head-type (treesit-node-type head-node))
                           (head-name (treesit-node-text head-node)))
                 (and (string= "par_tup_lit" parent-type)
                      (string= "sym_lit" head-type)
                      (if name
                          (string= name head-name)
                        t)))))))
      (when call-tuple-node
        (list (treesit-node-start call-tuple-node)
              (treesit-node-end call-tuple-node)
              ;; from second child onward
              (treesit-node-start
               (treesit-node-child call-tuple-node 1 :named))
              (1- (treesit-node-end call-tuple-node)))))))

(defun janet-ts--unwrap (&optional name)
  "Remove a form containing point, possibly climbing up to decide bounds.

Optional argument NAME bounds the upward ancestor searching.

If NAME is not provided, then do not climb up beyond the innermost
containing call form."
  (when-let ((result (janet-ts--wrap-calculate name)))
    (let (;; outer region
          (o-start (nth 0 result))
          (o-end (nth 1 result))
          ;; inner region
          (i-start (nth 2 result))
          (i-end (nth 3 result)))
      ;; copy
      (kill-ring-save i-start i-end)
      ;; delete
      (kill-region o-start o-end)
      ;; insert
      (yank 2)
      ;; tidy
      (indent-region o-start (point)))))

(defun janet-ts-unwrap-call-form ()
  "Unwrap innermost call form containing point."
  (interactive)
  (janet-ts--unwrap))

(defun janet-ts-unwrap-tracev-call ()
  "Unwrap suitably chosen form in tracev call."
  (interactive)
  (janet-ts--unwrap "tracev"))

(defun janet-ts-doc-and-usages-for-here ()
  "Show documentation and usages for a thing at point."
  (interactive)
  (when-let* ((result (janet-ts--bounds-calculate))
              (beg (nth 0 result))
              (end (nth 1 result))
              (thing (buffer-substring-no-properties beg end)))
    ;; https://github.com/sogaiu/janet-ref
    (shell-command (format "jref \"%s\"" thing))))

(defun janet-ts-doc-for-here ()
  "Show documentation for a thing at point."
  (interactive)
  (when-let* ((result (janet-ts--bounds-calculate))
              (beg (nth 0 result))
              (end (nth 1 result))
              (thing (buffer-substring-no-properties beg end)))
    ;; https://github.com/sogaiu/janet-ref
    (shell-command (format "jref -d \"%s\"" thing))))

(defun janet-ts-usages-for-here ()
  "Show usages for a thing at point."
  (interactive)
  (when-let* ((result (janet-ts--bounds-calculate))
              (beg (nth 0 result))
              (end (nth 1 result))
              (thing (buffer-substring-no-properties beg end)))
    ;; https://github.com/sogaiu/janet-ref
    (shell-command (format "jref -u \"%s\"" thing))))

(defun janet-ts-source-for-here ()
  "Show source for a thing at point."
  (interactive)
  (when-let* ((result (janet-ts--bounds-calculate))
              (beg (nth 0 result))
              (end (nth 1 result))
              (thing (buffer-substring-no-properties beg end)))
    ;; https://github.com/sogaiu/janet-ref
    (shell-command (format "jref -s \"%s\"" thing))))

(defun janet-ts-pdoc-and-usages-for-here ()
  "Show PEG documentation and usages for a thing at point."
  (interactive)
  (when-let* ((result (janet-ts--bounds-calculate))
              (beg (nth 0 result))
              (end (nth 1 result))
              (thing (buffer-substring-no-properties beg end)))
    ;; https://github.com/sogaiu/janet-pegdoc
    (shell-command (format "pdoc \"%s\"" thing))))

(defun janet-ts-pdoc-for-here ()
  "Show PEG documentation for a thing at point."
  (interactive)
  (when-let* ((result (janet-ts--bounds-calculate))
              (beg (nth 0 result))
              (end (nth 1 result))
              (thing (buffer-substring-no-properties beg end)))
    ;; https://github.com/sogaiu/janet-pegdoc
    (shell-command (format "pdoc -d \"%s\"" thing))))

(defun janet-ts-pdoc-usages-for-here ()
  "Show PEG usages for a thing at point."
  (interactive)
  (when-let* ((result (janet-ts--bounds-calculate))
              (beg (nth 0 result))
              (end (nth 1 result))
              (thing (buffer-substring-no-properties beg end)))
    ;; https://github.com/sogaiu/janet-pegdoc
    (shell-command (format "pdoc -u \"%s\"" thing))))

;; XXX: likely a better way to do this

;; https://www.gnu.org/software/emacs/manual/html_node/efaq/ \
;;         Modifying-pull_002ddown-menus.html
;; https://emacs.stackexchange.com/questions/15093/ \
;;         how-to-add-an-item-to-the-menu-bar

(define-key-after janet-ts-mode-map
  [menu-bar janet-ts sep-before-select]
  '(menu-item "--"))

(define-key-after janet-ts-mode-map
  [menu-bar janet-ts s-item]
  '("Select Around Point" . janet-ts-select))

(define-key-after janet-ts-mode-map
  [menu-bar janet-ts es-item]
  '("Expand Selection" . janet-ts-expand-selection))

(define-key-after janet-ts-mode-map
  [menu-bar janet-ts sep-before-format]
  '(menu-item "--"))

(define-key-after janet-ts-mode-map
  [menu-bar janet-ts fsap-item]
  '("Format Selection - Split At Parens" .
    janet-ts-format-selection-split-at-parens))

(define-key-after janet-ts-mode-map
  [menu-bar janet-ts fp-item]
  '("Format Selection - Pairs" . janet-ts-format-selection-pairs))

(define-key-after janet-ts-mode-map
  [menu-bar janet-ts sep-before-wrap]
  '(menu-item "--"))

(define-key-after janet-ts-mode-map
  [menu-bar janet-ts ucf-item]
  '("Unwrap Call Form" . janet-ts-unwrap-call-form))

(define-key-after janet-ts-mode-map
  [menu-bar janet-ts witc-item]
  '("Wrap In Tracev Call" . janet-ts-wrap-in-tracev-call))

(define-key-after janet-ts-mode-map
  [menu-bar janet-ts utc-item]
  '("Unwrap Tracev Call" . janet-ts-unwrap-tracev-call))

(define-key-after janet-ts-mode-map
  [menu-bar janet-ts sep-before-comment]
  '(menu-item "--"))

(define-key-after janet-ts-mode-map
  [menu-bar janet-ts tcb-item]
  '("Toggle Comment Blocks" . janet-ts-toggle-comment-blocks))

(define-key-after janet-ts-mode-map
  [menu-bar janet-ts tls-item]
  '("Toggle Long-Strings" . janet-ts-toggle-long-strings))

(define-key-after janet-ts-mode-map
  [menu-bar janet-ts sep-before-cycle]
  '(menu-item "--"))

(define-key-after janet-ts-mode-map
  [menu-bar janet-ts cd-item]
  '("Cycle Delimiters" . janet-ts-cycle-delimiters))

(define-key-after janet-ts-mode-map
  [menu-bar janet-ts mrdr-item]
  '("Move Right Delimiter Right" . janet-ts-move-right-delim-right))

(define-key-after janet-ts-mode-map
  [menu-bar janet-ts sep-before-struct]
  '(menu-item "--"))

(define-key-after janet-ts-mode-map
  [menu-bar janet-ts swn-item]
  '("Swap With Next" . janet-ts-swap-with-next))

;; node info in mode line
(add-hook 'janet-ts-mode-hook 'treesit-inspect-mode)

(provide 'janet-ts-experiment)
;;; janet-ts-experiment.el ends here
