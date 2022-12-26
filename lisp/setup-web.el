;;; setup-js.el --- Web/JS setup / options
;;; Commentary:
;;; Code:

(use-package typescript-mode
  :after tree-sitter
  :config
  ;; we choose this instead of tsx-mode so that eglot can automatically figure out language for server
  ;; see https://github.com/joaotavora/eglot/issues/624 and https://github.com/joaotavora/eglot#handling-quirky-servers
  (define-derived-mode typescriptreact-mode typescript-mode
    "TypeScript TSX")

  ;; use our derived mode for tsx files
  (add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescriptreact-mode))
  ;; by default, typescript-mode is mapped to the treesitter typescript parser
  ;; use our derived mode to map both .tsx AND .ts -> typescriptreact-mode -> treesitter tsx
  (add-to-list 'tree-sitter-major-mode-language-alist '(typescriptreact-mode . tsx)))

;; https://github.com/orzechowskid/tsi.el/
;; great tree-sitter-based indentation for typescript/tsx, css, json
(use-package tsi
  :after tree-sitter
  :quelpa (tsi :fetcher github :repo "orzechowskid/tsi.el")
  ;; define autoload definitions which when actually invoked will cause package to be loaded
  :commands (tsi-typescript-mode tsi-json-mode tsi-css-mode)
  :init
  (add-hook 'typescript-mode-hook (lambda () (tsi-typescript-mode 1)))
  (add-hook 'json-mode-hook (lambda () (tsi-json-mode 1)))
  (add-hook 'css-mode-hook (lambda () (tsi-css-mode 1)))
  (add-hook 'scss-mode-hook (lambda () (tsi-scss-mode 1))))

;; (use-package apheleia
;;   :ensure t
;;   :config
;;   (apheleia-global-mode +1))


;; (use-package prettier-js)

;; (use-package whitespace-cleanup-mode)

;; (use-package helm-dash)
;; (use-package tagedit)
;; (use-package pretty-mode)

;; (req-package web-mode
;;   :mode (("\\.jsx" . web-mode)
;;          ("\\.js$" . web-mode)
;;          ("\\.ts$" . web-mode)
;;          ("\\.tsx$" . web-mode)
;;          ("\\.html$" . web-mode)
;;          ("\\.vue$" . web-mode))
;;   :init
;;   (setq web-mode-markup-indent-offset 2)
;;   (setq web-mode-css-indent-offset 2)
;;   (setq web-mode-code-indent-offset 2)
;;   (setq web-mode-enable-current-column-highlight t)
;;   :config
;;   ;; Support JSX in regular javascript files
;;   (setq web-mode-content-types-alist
;;         '(("jsx" . "\\.js[x]?\\'")))
;;   ;; Use Django by default in html templates
;;   (setq web-mode-engines-alist
;;         '(("django"    . "\\.html\\'")))
;;   ;; Setup indentation
;;   (add-to-list 'web-mode-indentation-params '("lineup-args" . nil))
;;   (add-to-list 'web-mode-indentation-params '("lineup-calls" . nil))
;;   (add-to-list 'web-mode-indentation-params '("lineup-concats" . nil))
;;   (add-to-list 'web-mode-indentation-params '("lineup-quotes" . nil))
;;   (add-to-list 'web-mode-indentation-params '("lineup-ternary" . nil))
;;   ;; Use eslint
;;   (flycheck-add-mode 'javascript-eslint 'web-mode)
;;   (flycheck-add-next-checker 'javascript-eslint 'jsx-tide 'append)
;;   ;; Enable pretty-mode
;;   (add-to-list 'pretty-modes-aliases '(web-mode . javascript-mode))
;;   ;; Use tagedit
;;   (add-hook 'web-mode-hook
;;             (lambda ()
;;               (tagedit-mode t)
;;               (flycheck-mode t)
;;               (prettier-js-mode t)
;;               (whitespace-cleanup-mode t)
;;               (when (string-equal "tsx" (file-name-extension buffer-file-name))
;;                 (setup-tide-mode))
;;               ;; (setq-local helm-dash-docsets '("JavaScript" "React"))
;;               )))

;; ;; (req-package tern
;; ;;   :require web-mode
;; ;;   :config
;; ;;   (add-hook 'web-mode-hook
;; ;;             (lambda ()
;; ;;               (tern-mode t))))

;; ;; (req-package company-tern
;; ;;   :require company
;; ;;   :config
;; ;;   (add-to-list 'company-backends 'company-tern))

;; (use-package css-mode)

;; (use-package scss-mode)

;; (use-package json-mode
;;   :mode (("\\.json" . json-mode))
;;   :config
;;   (add-hook 'json-mode-hook
;;             (lambda ()
;;               (make-local-variable 'js-indent-level)
;;               (setq js-indent-level 2))))

;; (use-package typescript-mode
;;   :config
;;   (setq typescript-indent-level 2))

;; (use-package tide
;;   :after (typescript-mode company flycheck)
;;   :config
;;   (defun setup-tide-mode ()
;;     (interactive)
;;     (tide-setup)
;;     (flycheck-mode +1)
;;     (setq flycheck-check-syntax-automatically '(save mode-enabled))
;;     (eldoc-mode +1)
;;     (tide-hl-identifier-mode +1)
;;     (company-mode +1))
;;   (add-hook 'typescript-mode-hook #'setup-tide-mode)
;;   ;; (flycheck-add-mode 'typescript-tslint 'web-mode)
;; )

(provide 'setup-web)
;;; setup-web.el ends here
