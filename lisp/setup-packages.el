;;; setup-packages.el --- General packages setup
;;; Commentary:
;;; Code:
(require 'req-package)
(defvar dotfiles-dir)

;; Use twilight theme
(req-package material-theme)

;; Make buffer names unique
(req-package uniquify
  :force t
  :config
  (setq uniquify-buffer-name-style 'forward))

;; Load exec paths from the shell
(req-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "SSH_AGENT_PID")
  (exec-path-from-shell-copy-env "SSH_AUTH_SOCK"))

;; Powerline
(req-package smart-mode-line
  :config
  (sml/setup))

(req-package smart-mode-line-powerline-theme
  :require smart-mode-line
  :config
  (setq sml/theme 'smart-mode-line-powerline))

;; Undo tree
(req-package undo-tree
  :diminish 'undo-tree-mode
  :config
  (global-undo-tree-mode)
  ;; Keep region when undoing in region
  (defadvice undo-tree-undo (around keep-region activate)
    (if (use-region-p)
        (let ((m (set-marker (make-marker) (mark)))
              (p (set-marker (make-marker) (point))))
          ad-do-it
          (goto-char p)
          (set-mark m)
          (set-marker p nil)
          (set-marker m nil))
      ad-do-it)))

;; Flycheck configuration
(req-package flycheck
  :config
  ;; Launch flycheck when opening a file
  (add-hook 'find-file-hook 'flycheck-mode)
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers '(javascript-jshint))))

  ;; Enable flycheck-pos-tip
(req-package flycheck-pos-tip
  :require flycheck
  :config
  (flycheck-pos-tip-mode))

;; Save history in mini buffers
(req-package savehist
  :config
  (setq savehist-file (concat dotfiles-dir "savehist"))
  (savehist-mode t)
  (setq history-length t)
  (setq history-delete-duplicates t)
  (setq savehist-additional-variables
        '(kill-ring
          search-ring
          regexp-search-ring)))

;; Use switch-window
(req-package switch-window
  :bind ("C-x o" . switch-window))

;; Use git-gutter
(req-package git-gutter-fringe
  :if window-system
  :diminish 'git-gutter-mode
  :config
  (global-git-gutter-mode)
  (setq-default indicate-buffer-boundaries 'left)
  (setq-default indicate-empty-lines t))

;; Use helm
(req-package helm
  :demand t
  :bind (("M-x" . helm-M-x)
         ("M-y" . helm-show-kill-ring)
         ("C-x b" . helm-mini)
         ("C-x C-f" . helm-find-files)
         ("C-x f" . helm-recentf)
         ("C-h a" . helm-apropos))
  :functions helm-autoresize-mode
  :config
  (helm-mode t)
  (helm-autoresize-mode t))

(req-package helm-flx
  :require helm
  :config
  (helm-flx-mode t))

(req-package helm-flycheck
  :require flycheck helm
  :bind (:map flycheck-mode-map
              ("C-c ! h" . helm-flycheck)))

(req-package helm-dash
  :require helm
  :config
  (setq helm-dash-common-docsets '("AWS_JavaScript" "Elasticsearch")))

(req-package helm-flyspell
  :require helm flyspell
  :config
  (bind-key* "C-;" 'helm-flyspell-correct))

;; Use projectile and helm-projectile
(req-package projectile
  :config
  (projectile-global-mode t)
  (setq projectile-enable-caching t))

(req-package helm-projectile
  :require helm projectile
  :bind ("C-c h" . helm-projectile))

(req-package helm-ag
  :require helm projectile
  :bind ("C-c p s a" . helm-projectile-ag))

;; Magit
(req-package magit
  :bind ("C-x g" . magit-status))

(req-package magithub
  :require magit
  :config
  (magithub-feature-autoinject t)
  (defun magithub--url->domain (url)
    "Tries to parse a remote url into a domain"
    (cdr (assq 'domain (magithub--parse-url url))))

  (add-hook 'magit-status-mode-hook '(lambda ()
                                       (if (magithub-github-repository-p)
                                           (let* ((remote-url (magit-get "remote" (magithub-source--remote) "url"))
                                                  (domain (magithub--url->domain remote-url)))
                                             (message domain)
                                             (unless (string-equal "github.com" domain)
                                               (setq-local ghub-base-url (concat "https://" domain "/api/v3"))))))))

;; Avy for fast jumping
  (req-package avy
    :bind ("C-c j" . avy-goto-word-or-subword-1))

  ;; Use rainbow-delimiters to display unbalanced delimiters
  ;; Ref: http://timothypratley.blogspot.no/2015/07/seven-specialty-emacs-settings-with-big.html
  (req-package rainbow-delimiters
    :config
    (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
    (set-face-attribute 'rainbow-delimiters-unmatched-face nil
                        :foreground 'unspecified
                        :inherit 'error))

;; Which key
(req-package which-key
  :config
  (which-key-mode t))

;; Agressive indent mode
(req-package aggressive-indent
  :config
  (global-aggressive-indent-mode t)
  (add-to-list 'aggressive-indent-excluded-modes 'web-mode))

;; Pretty mode
(req-package pretty-mode
  :config
  (global-pretty-mode t))

;; Expand region
(req-package expand-region
  :bind ("C-=" . er/expand-region))

;; Multiple cursor
(req-package multiple-cursors
  :bind (("C-c C-SPC" . set-rectangular-region-anchor)
         ("C-ø n" . mc/mark-next-like-this)
         ("C-ø p" . mc/mark-previous-like-this)
         ("C-ø a" . mc/mark-all-like-this)))

;; Dumb jump
(req-package dumb-jump
  :config
  (dumb-jump-mode))

;; Langtool
(req-package langtool
  :bind (("C-x M-l c" . langtool-check)
         ("C-x M-l d" . langtool-check-done)
         ("C-x M-l l" . langtool-switch-default-language)
         ("C-x M-l s" . langtool-correct-buffer))
  :init
  (setq langtool-java-classpath "/usr/share/languagetool:/usr/share/java/languagetool/*")
  (setq langtool-default-language "en-US"))

;; Company
(req-package company
  :require company-quickhelp
  :config
  (company-mode t)
  (add-hook 'after-init-hook 'global-company-mode)
  (company-quickhelp-mode t)
  (setq company-tooltip-align-annotations t))

;; Ispell
(req-package ispell
  :init
  ;; Support aspell in flyspell
  (setq ispell-list-command "--list"))

;; Flyspell
(req-package flyspell
  :config
  (add-hook 'prog-mode-hook 'flyspell-prog-mode))

;; Markdown
(req-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode ("\\.md" . gfm-mode))

(req-package gh-md
  :require markdown-mode
  :bind (:map markdown-mode-map
              ("C-c C-p" . gh-md-render-buffer)))

;; Lua
(req-package lua-mode
  :mode ("\\.lua" . lua-mode))

;; Elasticsearch
(req-package es-mode
  :commands (es-mode))

;; Smartparens
(req-package smartparens
  :diminish smartparens-mode
  :config
  (require 'smartparens-config)
  (smartparens-global-mode t))

;; Rainbow
(req-package rainbow-mode)

;; Yaml mode
(req-package yaml-mode)

;; Go
(req-package go-mode
  :require company-go
  :config
  (setq gofmt-command "goimports")
  (add-to-list 'company-backends 'company-go)
  (add-hook 'before-save-hook 'gofmt-before-save))

(provide 'setup-packages)
;;; setup-packages.el ends here
