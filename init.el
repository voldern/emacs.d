;;; init.el --- Initial emacs configuration file
;;; Commentary:
;;; Code:
(setq package-enable-at-startup nil)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(use-package use-package-ensure-system-package
  :straight t
  :ensure t)

;; Load configurations
(defconst dotfiles-dir (file-name-directory
                        (or (buffer-file-name) load-file-name)))
(add-to-list 'load-path (concat dotfiles-dir "lisp"))

;;(setq custom-file (concat dotfiles-dir "custom.el"))
;;(load custom-file)

(require 'setup-general)
(require 'setup-packages)
(require 'setup-web)
;;(require 'setup-org)
(require 'setup-rust)

(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default))
 '(package-selected-packages
   '(go-mode terraform-mode dockerfile-mode php-mode yasnippet keychain-environment yaml-mode which-key use-package-ensure-system-package undo-tree typescript-mode tsi tree-sitter-langs smartparens smart-mode-line-powerline-theme rustic rainbow-mode rainbow-delimiters racer quelpa-use-package multiple-cursors material-theme magit lsp-ui helm-xref helm-rg helm-projectile helm-lsp helm-flycheck graphql-mode git-gutter-fringe expand-region exec-path-from-shell diminish dashboard company-racer company-quickhelp avy apheleia aggressive-indent))
 '(warning-suppress-types '((lsp-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
