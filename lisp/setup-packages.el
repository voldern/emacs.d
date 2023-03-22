;;; setup-packages.el --- General packages setup
;;; Commentary:
;;; Code:
;; (defvar dotfiles-dir)

;; ;; Use twilight theme
(use-package material-theme
  :ensure t
  :config
  (load-theme 'material t))

(use-package diminish)

;; Dashboard
(use-package dashboard
  :ensure t
  :init
  (setq dashboard-projects-backend 'projectile)
  :config
  (dashboard-setup-startup-hook))

;; Load exec paths from the shell
(use-package exec-path-from-shell
  :config
  (dolist (var '("PATH"))
    (add-to-list 'exec-path-from-shell-variables var))
  (setq exec-path-from-shell-arguments 'nil)
  (exec-path-from-shell-initialize))

(exec-path-from-shell-initialize)

(use-package keychain-environment
  :config
  (keychain-refresh-environment))

;; Powerline
(use-package smart-mode-line
  :config
  (sml/setup))

(use-package smart-mode-line-powerline-theme
  :config
  (setq sml/theme 'smart-mode-line-powerline))

;; Undo tree
(use-package undo-tree
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode))

;; ;; Undo tree
;; ;(req-package undo-tree
;; ;  :diminish 'undo-tree-mode
;; ;  :config
;; ;  (global-undo-tree-mode)
;; ;  ;; Keep region when undoing in region
;; ;  (defadvice undo-tree-undo (around keep-region activate)
;; ;    (if (use-region-p)
;; ;        (let ((m (set-marker (make-marker) (mark)))
;; ;              (p (set-marker (make-marker) (point))))
;; ;          ad-do-it
;; ;          (goto-char p)
;; ;          (set-mark m)
;; ;          (set-marker p nil)
;; ;          (set-marker m nil))
;; ;      ad-do-it)))

;; ;; Flycheck configuration
;; (use-package flycheck
;;   :config
;;   ;; Launch flycheck when opening a file
;;   (add-hook 'find-file-hook 'flycheck-mode)
;;   (setq-default flycheck-disabled-checkers
;;                 (append flycheck-disabled-checkers '(javascript-jshint))))

;;   ;; Enable flycheck-pos-tip
;; (use-package flycheck-pos-tip
;;   :config
;;   (flycheck-pos-tip-mode))

;; ;; Save history in mini buffers
;; (use-package savehist
;;   :config
;;   (setq savehist-file (concat dotfiles-dir "savehist"))
;;   (savehist-mode t)
;;   (setq history-length t)
;;   (setq history-delete-duplicates t)
;;   (setq savehist-additional-variables
;;         '(kill-ring
;;           search-ring
;;           regexp-search-ring)))

;; ;; Use git-gutter
(use-package git-gutter-fringe
  :if window-system
  :diminish 'git-gutter-mode
  :config
  (global-git-gutter-mode)
  (setq-default indicate-buffer-boundaries 'left)
  (setq-default indicate-empty-lines t))

;; Use helm
(use-package helm
  :bind (("M-x" . helm-M-x)
         ("M-y" . helm-show-kill-ring)
         ("C-x b" . helm-mini)
         ("C-x C-f" . helm-find-files)
         ("C-x f" . helm-recentf)
         ("C-h a" . helm-apropos))
  :config
  (helm-mode t)
  (helm-autoresize-mode t))

(use-package helm-rg
  :ensure-system-package
  (rg . ripgrep))

;; (use-package helm-flx
;;   :config
;;   (helm-flx-mode t))

;; (use-package helm-flycheck
;;   :bind (:map flycheck-mode-map
;;               ("C-c ! h" . helm-flycheck)))


;; (use-package helm-flyspell
;;   :config
;;   (bind-key* "C-;" 'helm-flyspell-correct))

;; ;; Use projectile and helm-projectile
(use-package projectile
  :config
  (projectile-global-mode t)
  (setq projectile-enable-caching t))

(use-package helm-projectile
  :bind (("C-c h" . helm-projectile)
         ("C-c p r" . helm-projectile-rg)))

;; Magit
(use-package magit
  :bind ("C-x g" . magit-status))

;; (use-package forge
;;   :after magit)

;; ;; (req-package magithub
;; ;;   :require magit
;; ;;   :config
;; ;;   (magithub-feature-autoinject t)
;; ;;   (defun magithub--url->domain (url)
;; ;;     "Tries to parse a remote url into a domain"
;; ;;     (cdr (assq 'domain (magithub--parse-url url))))

;; ;;   (add-hook 'magit-status-mode-hook '(lambda ()
;; ;;                                        (if (magithub-github-repository-p)
;; ;;                                            (let* ((remote-url (magit-get "remote" (magithub-source--remote) "url"))
;; ;;                                                   (domain (magithub--url->domain remote-url)))
;; ;;                                              (message domain)
;; ;;                                              (unless (string-equal "github.com" domain)
;; ;;                                                (setq-local ghub-base-url (concat "https://" domain "/api/v3")))))))
;; ;;   )

;; Avy for fast jumping
(use-package avy
  :bind ("C-c j" . avy-goto-word-or-subword-1))

;; Use rainbow-delimiters to display unbalanced delimiters
(use-package rainbow-delimiters
  :hook prog-mode
  :config
  (set-face-attribute 'rainbow-delimiters-unmatched-face nil
                      :foreground 'unspecified
                      :inherit 'error))

;; Which key
(use-package which-key
  :config
  (which-key-mode t))

;; ;; Agressive indent mode
;; (use-package aggressive-indent
;;   :config
;;   (global-aggressive-indent-mode t)
;;   (add-to-list 'aggressive-indent-excluded-modes 'web-mode))

;; ;; Pretty mode
;; ;; (use-package pretty-mode
;; ;;   :config
;; ;;   (global-pretty-mode t))

;; Expand region
(use-package expand-region
  :bind ("C-=" . er/expand-region))

;; Multiple cursor
(use-package multiple-cursors
  :bind (("C-c C-SPC" . set-rectangular-region-anchor)
         ("C-ø e" . mc/edit-lines)
         ("C-ø n" . mc/mark-next-like-this)
         ("C-ø p" . mc/mark-previous-like-this)
         ("C-ø a" . mc/mark-all-like-this)))

;; ;; ;; Dumb jump
;; (use-package dumb-jump
;;   :config
;;   (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

;; ;; Langtool
;; (use-package langtool
;;   :bind (("C-x M-l c" . langtool-check)
;;          ("C-x M-l d" . langtool-check-done)
;;          ("C-x M-l l" . langtool-switch-default-language)
;;          ("C-x M-l s" . langtool-correct-buffer))
;;   :init
;;   (setq langtool-java-classpath "/usr/share/languagetool:/usr/share/java/languagetool/*")
;;   (setq langtool-default-language "en-US"))

;; Company
(use-package company
  :ensure t
  :custom
  (company-idle-delay 0)
  ;; (company-minimum-prefix-length 1)
  ;; (company-tooltip-align-annotations t)
  :bind
  (:map company-active-map
	    ("C-n". company-select-next)
	    ("C-p". company-select-previous)
	    ("M-<". company-select-first)
	    ("M->". company-select-last)
        :map company-mode-map
	    ("<tab>". tab-indent-or-complete)
	    ("TAB". tab-indent-or-complete)))

(defun check-expansion ()
  (save-excursion
    (if (looking-at "\\_>") t
      (backward-char 1)
      (if (looking-at "\\.") t
        (backward-char 1)
        (if (looking-at "::") t nil)))))

(defun tab-indent-or-complete ()
  (interactive)
  (if (minibufferp)
      (minibuffer-complete)
    (if (check-expansion)
        (company-complete-common)
      (indent-for-tab-command))))


(use-package company-quickhelp
  :after company
  :init (company-quickhelp-mode))

;; Flycheck
(use-package flycheck
  :ensure t)

(use-package helm-flycheck
  :after flycheck
  :bind
  ("C-c ! h" . helm-flycheck))

;; (use-package tree-sitter
;;   :ensure t
;;   :config
;;   (global-tree-sitter-mode)
;;   (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

;; (use-package tree-sitter-langs
;;   :ensure t
;;   :after tree-sitter)

;; ;; Ispell
;; (use-package ispell
;;   :init
;;   ;; Support aspell in flyspell
;;   (setq ispell-list-command "--list"))

;; ;; Flyspell
;; (use-package flyspell
;;   :config
;;   (add-hook 'prog-mode-hook 'flyspell-prog-mode))

;; ;; Markdown
;; (use-package markdown-mode
;;   :commands (markdown-mode gfm-mode)
;;   :mode ("\\.md" . gfm-mode))

;; (use-package gh-md
;;   :bind (:map markdown-mode-map
;;               ("C-c C-p" . gh-md-render-buffer)))

;; ;; Lua
;; (use-package lua-mode
;;   :mode ("\\.lua" . lua-mode))

;; ;; Elasticsearch
;; (use-package es-mode
;;   :commands (es-mode))

;; Smartparens
(use-package smartparens
  :diminish smartparens-mode
  :config
  (require 'smartparens-config)
  (smartparens-global-mode t))

;; Rainbow
(use-package rainbow-mode)

;; Yaml mode
(use-package yaml-mode)

;; Yasnippet
(use-package yasnippet
  :hook (prog-mode . yas-minor-mode))

;; LSP
(use-package lsp-mode
  :ensure t
  :commands lsp
  :hook (lsp-ui
         (typescript-mode . lsp)
         (php-mode . lsp))
  :init
  (setq lsp-keymap-prefix "C-c l")
  :custom
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.6)
  (lsp-rust-analyzer-server-display-inlay-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  (lsp-rust-analyzer-display-chaining-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
  (lsp-rust-analyzer-display-closure-return-type-hints t)
  (lsp-rust-analyzer-display-parameter-hints nil)
  (lsp-rust-analyzer-display-reborrow-hints nil)
  :config
  (lsp-register-custom-settings
   '(("gopls.completeUnimported" t t)
     ("gopls.staticcheck" t t))))

(use-package lsp-ui
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable nil))

(use-package helm-xref
  :ensure t
  :after helm)

(use-package helm-lsp
  :commands helm-lsp-workspace-symbol
  :bind (:map lsp-mode-map ([remap xref-find-apropos] . helm-lsp-workspace-symbol)))

;; ;; Go
;; (use-package go-mode
;;   :config
;;   ;; (setq gofmt-command "goimports")
;;   (add-to-list 'company-backends 'company-lsp)
;;   ;; (add-hook 'before-save-hook 'gofmt-before-save)
;;   (add-hook 'before-save-hook 'lsp-format-buffer)
;;   (add-hook 'before-save-hook 'lsp-organize-imports)
;;   )

;; (use-package elpy
;;   :init
;;   (elpy-enable))

;; (req-package pyenv-mode
;;   :config
;;   (defun projectile-pyenv-mode-set ()
;;     "Set pyenv version matching project name."
;;     (let ((project (projectile-project-name)))
;;       (if (member project (pyenv-mode-versions))
;;           (pyenv-mode-set project)
;;         (pyenv-mode-unset))))
;;   (add-hook 'projectile-after-switch-project-hook 'projectile-pyenv-mode-set))

(provide 'setup-packages)
;; ;;; setup-packages.el ends here
