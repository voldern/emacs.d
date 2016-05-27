;;; setup-js.el --- Web/JS setup / options
;;; Commentary:
;;; Code:
(require 'req-package)

(req-package web-mode
  :require flycheck pretty-mode tagedit
  :mode (("\\.jsx" . web-mode)
         ("\\.js" . web-mode))
  :config
  ;; Support JSX in regular javascript files
  (setq web-mode-content-types-alist
        '(("jsx" . "\\.js[x]?\\'")))
  ;; Setup indentation
  (add-to-list 'web-mode-indentation-params '("lineup-args" . nil))
  (setq web-mode-markup-indent-offset 4)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 4)
  ;; Use eslint
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  ;; Enable pretty-mode
  (add-to-list 'pretty-modes-aliases '(web-mode . javascript-mode))
  ;; Use tagedit
  (add-hook 'web-mode-hook
            (lambda ()
              (tagedit-mode t))))

(req-package tern
  :require auto-complete tern-auto-complete web-mode
  :config
  (add-hook 'web-mode-hook
            (lambda ()
              (tern-mode t)))
  (tern-ac-setup))

(req-package css-mode
  :config
  (add-hook 'css-mode-hook 'syntax-color-hex))

(req-package scss-mode
  :config
  (add-hook 'scss-mode-hook 'syntax-color-hex))

(req-package json-mode
  :mode (("\\.json" . json-mode)))

(defun syntax-color-hex ()
  "Syntax color hex color spec such as 「#ff1100」 in current buffer."
  (interactive)
  (font-lock-add-keywords
   nil
   '(("\\(#\\)[abcdef[:digit:]]\\{6\\}"
      (0 (put-text-property
          (match-beginning 1)
          (match-end 1)
          'face (list :background (match-string-no-properties 0)))))))
  (font-lock-fontify-buffer))

(provide 'setup-web)
;;; setup-web.el ends here
