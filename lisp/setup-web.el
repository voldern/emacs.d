;;; setup-js.el --- Web/JS setup / options
;;; Commentary:
;;; Code:
(require 'req-package)

(req-package web-mode
  :require flycheck pretty-mode tagedit rainbow-mode
  :mode (("\\.jsx" . web-mode)
         ("\\.js" . web-mode))
  :config
  ;; Support JSX in regular javascript files
  (setq web-mode-content-types-alist
        '(("jsx" . "\\.js[x]?\\'")))
  ;; Setup indentation
  (add-to-list 'web-mode-indentation-params '("lineup-args" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-ternary" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-concats" . nil))
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
              (tagedit-mode t)
              (rainbow-mode t))))

(req-package tern
  :require auto-complete tern-auto-complete web-mode
  :config
  (add-hook 'web-mode-hook
            (lambda ()
              (tern-mode t)))
  (tern-ac-setup))

(req-package css-mode
  :require rainbow-mode
  :config
  (add-hook 'css-mode-hook 'rainbow-mode))

(req-package scss-mode
  :require rainbow-mode
  :config
  (add-hook 'scss-mode-hook 'rainbow-mode))

(req-package json-mode
  :mode (("\\.json" . json-mode)))

(provide 'setup-web)
;;; setup-web.el ends here
