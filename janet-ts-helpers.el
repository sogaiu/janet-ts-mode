;;; janet-ts-helpers --- Use external helpers

;;; Commentary:

;;; Code:

(require 'janet-ts-mode)

(defun janet-ts-doc-for-here ()
  "Show documentation for a thing at point."
  (interactive)
  (when-let* ((result (janet-ts--bounds-calculate))
              (beg (nth 0 result))
              (end (nth 1 result))
              (thing (buffer-substring-no-properties beg end)))
    ;; https://github.com/sogaiu/jdoc
    (shell-command (format "jdoc \"%s\"" thing))))

(defun janet-ts-usages-for-here ()
  "Show usages for a thing at point."
  (interactive)
  (when-let* ((result (janet-ts--bounds-calculate))
              (beg (nth 0 result))
              (end (nth 1 result))
              (thing (buffer-substring-no-properties beg end)))
    ;; https://github.com/sogaiu/janet-ref
    (shell-command (format "jref -u \"%s\"" thing))))

(defun janet-ts-doc-and-usages-for-here ()
  "Show documentation and usages for a thing at point."
  (interactive)
  (when-let* ((result (janet-ts--bounds-calculate))
              (beg (nth 0 result))
              (end (nth 1 result))
              (thing (buffer-substring-no-properties beg end)))
    ;; https://github.com/sogaiu/janet-ref
    (shell-command (format "jref \"%s\"" thing))))

(defun janet-ts-pdoc-for-here ()
  "Show PEG documentation for a thing at point."
  (interactive)
  (when-let* ((result (janet-ts--bounds-calculate))
              (beg (nth 0 result))
              (end (nth 1 result))
              (thing (buffer-substring-no-properties beg end)))
    ;; XXX
    (message "pdoc -d \"%s\"" thing)
    (message "%s" (format "pdoc -d \"%s\"" thing))
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

(defun janet-ts-pdoc-and-usages-for-here ()
  "Show PEG documentation and usages for a thing at point."
  (interactive)
  (when-let* ((result (janet-ts--bounds-calculate))
              (beg (nth 0 result))
              (end (nth 1 result))
              (thing (buffer-substring-no-properties beg end)))
    ;; https://github.com/sogaiu/janet-pegdoc
    (shell-command (format "pdoc \"%s\"" thing))))

'(defun janet-ts-source-for-here ()
  "Show source for a thing at point."
  (interactive)
  (when-let* ((result (janet-ts--bounds-calculate))
              (beg (nth 0 result))
              (end (nth 1 result))
              (thing (buffer-substring-no-properties beg end)))
    ;; https://github.com/sogaiu/janet-ref
    (shell-command (format "jref -s \"%s\"" thing))))

(define-key-after janet-ts-mode-map
  [menu-bar janet-ts sep-before-daufh]
  '(menu-item "--"))

(define-key-after janet-ts-mode-map
  [menu-bar janet-ts dfh-item]
  '("Doc for Here" . janet-ts-doc-for-here))

(define-key-after janet-ts-mode-map
  [menu-bar janet-ts ufh-item]
  '("Usages for Here" . janet-ts-usages-for-here))

(define-key-after janet-ts-mode-map
  [menu-bar janet-ts daufh-item]
  '("Doc and Usages for Here" . janet-ts-doc-and-usages-for-here))

(define-key-after janet-ts-mode-map
  [menu-bar janet-ts pdfh-item]
  '("PEG Doc for Here" . janet-ts-pdoc-for-here))

(define-key-after janet-ts-mode-map
  [menu-bar janet-ts pufh-item]
  '("PEG Usages for Here" . janet-ts-pdoc-usages-for-here))

(define-key-after janet-ts-mode-map
  [menu-bar janet-ts pdaufh-item]
  '("PEG Doc and Usages for Here" . janet-ts-pdoc-and-usages-for-here))

(provide 'janet-ts-helpers)
;;; janet-ts-helpers.el ends here
