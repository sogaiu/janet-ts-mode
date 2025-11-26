;;; janet-ts-eldoc --- Eldoc

;;; Commentary:

;;; Code:

(require 'janet-ts-mode)
(require 'treesit)
(require 'project)
(require 'subr-x)

(defvar janet-ts--eldoc-helper-path
  (expand-file-name
   (concat (expand-file-name
	    (file-name-directory (or load-file-name
				     buffer-file-name)))
           "eldoc.janet"))
  "Path to eldoc helper program.")

(defun janet-ts--eldoc-make-args-string (symbol-name)
  "Make an argument string for SYMBOL-NAME.
An argument string represents the signature for a function."
  (let ((result nil)
        (proj-root (project-root (project-current)))
        (temp-buffer (generate-new-buffer "*janet-ts-eldoc*")))
    ;; XXX: give feedback if project root could not be determined?
    (when proj-root
      (save-excursion
        (let ((exit-code
               ;; XXX: untested on windows
               (call-process janet-ts--eldoc-helper-path
                             nil temp-buffer nil
                             symbol-name proj-root)))
          (if (not (zerop exit-code))
              (progn
                (message "eldoc helper exit-code: %s" exit-code)
                (error "Non-zero exit"))
            (set-buffer temp-buffer)
            (setq result
                  (string-trim-right ; remove trailing newline
                   (buffer-substring-no-properties (point-min)
                                                   (point-max))))
            (kill-buffer temp-buffer))))
      result)))

(defun janet-ts-eldoc (callback &rest _)
  "Backend function for eldoc to show argument list in the echo area.
CALLBACK is called with information obtained from an external source."
  ;; prevent overwriting error message
  (when (not (member last-command '(next-error previous-error)))
    ;; obtain current relevant symbol
    (let* ((curr-node (treesit-node-at (point)))
           (parent (if (treesit-node-check curr-node 'named)
                       (treesit-node-parent curr-node)
                     ;; on anon node (e.g. open paren), up another level
                     (treesit-node-parent (treesit-node-parent curr-node))))
           (matches
            (treesit-filter-child parent
                                  (lambda (child)
                                    (string-equal "sym_lit"
                                                  (treesit-node-type child)))
                                  :named))
           (leftmost (car matches))
           (sym (when leftmost (treesit-node-text leftmost))))
      ;; may be call the callback with the info
      (when sym
        (let ((args-str (janet-ts--eldoc-make-args-string sym)))
          (when (not (string-equal "" args-str))
            (funcall callback args-str :thing sym)))))))

(defun janet-ts-eldoc-setup (&optional remove?)
  "Configure eldoc in the current buffer.
Optional REMOVE? argument turns off eldoc support."
  ;; for the versions of emacs supported, shouldn't need
  (when (boundp 'eldoc-documentation-functions)
    (if remove?
        (remove-hook 'eldoc-documentation-functions
                     #'janet-ts-eldoc t)
      (add-hook 'eldoc-documentation-functions #'janet-ts-eldoc nil t))))

(provide 'janet-ts-eldoc)
;;; janet-ts-eldoc.el ends here
