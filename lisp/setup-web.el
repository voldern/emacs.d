;;; setup-js.el --- Web/JS setup / options
;;; Commentary:
;;; Code:
(require 'req-package)

(req-package web-mode
  :require flycheck pretty-mode tagedit helm-dash whitespace-cleanup-mode prettier-js
  :mode (("\\.jsx" . web-mode)
         ("\\.js$" . web-mode)
         ("\\.ts$" . web-mode)
         ("\\.tsx$" . web-mode)
         ("\\.html$" . web-mode))
  :init
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-enable-current-column-highlight t)
  :config
  ;; Support JSX in regular javascript files
  (setq web-mode-content-types-alist
        '(("jsx" . "\\.js[x]?\\'")))
  ;; Use Django by default in html templates
  (setq web-mode-engines-alist
        '(("django"    . "\\.html\\'")))
  ;; Setup indentation
  (add-to-list 'web-mode-indentation-params '("lineup-args" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-calls" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-concats" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-quotes" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-ternary" . nil))
  ;; Use eslint
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  ;; Enable pretty-mode
  (add-to-list 'pretty-modes-aliases '(web-mode . javascript-mode))
  ;; Use tagedit
  (add-hook 'web-mode-hook
            (lambda ()
              (tagedit-mode t)
              (flycheck-mode t)
              (prettier-js-mode t)
              (whitespace-cleanup-mode t)
              (setq-local helm-dash-docsets '("JavaScript" "React")))))

(req-package tern
  :require web-mode
  :config
  (add-hook 'web-mode-hook
            (lambda ()
              (tern-mode t))))

(req-package company-tern
  :require company
  :config
  (add-to-list 'company-backends 'company-tern))

(req-package css-mode)

(req-package scss-mode)

(req-package json-mode
  :mode (("\\.json" . json-mode))
  :config
  (add-hook 'json-mode-hook
            (lambda ()
              (make-local-variable 'js-indent-level)
              (setq js-indent-level 2))))

(req-package typescript-mode
  :config
  (setq typescript-indent-level 2))

(req-package tide
  :require typescript-mode web-mode flycheck
  :hook web-mode
  :config
  (defun setup-tide-mode ()
    (interactive)
    (tide-setup)
    (flycheck-mode +1)
    (setq flycheck-check-syntax-automatically '(save mode-enabled))
    (eldoc-mode +1)
    (tide-hl-identifier-mode +1))
  (setup-tide-mode)
  (flycheck-add-mode 'typescript-tslint 'web-mode))

(provide 'setup-web)
;;; setup-web.el ends here
