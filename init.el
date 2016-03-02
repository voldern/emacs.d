;; Load path etc.
(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))

(add-to-list 'load-path (concat dotfiles-dir "lisp"))
(add-to-list 'load-path (concat dotfiles-dir "vendor"))
(add-to-list 'load-path (concat dotfiles-dir "vendor/phpplus-mode"))

;; ELPA
(require 'setup-packages)

;; Fonts
(when (member "Envy Code R" (font-family-list))
  (add-to-list 'default-frame-alist '(font .  "Envy Code R-12")))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(variable-pitch ((t (:family "Liberation Sans")))))

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(ansi-color-for-comint-mode-on)

;; Add private bin folder to loadpath
(setenv "PATH" (concat (getenv "HOME") "/bin:" (getenv "PATH")))
(setq exec-path (cons (concat (getenv "HOME") "/bin") exec-path))

;; Disable startup screen
(setq inhibit-startup-message t)

(setq xterm-mouse-mode t)

;; Disable menu
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Enable git-gutter on window systems
(when (window-system)
  (require 'git-gutter-fringe)
  (global-git-gutter-mode 1)
  (setq-default indicate-buffer-boundaries 'left)
  (setq-default indicate-empty-lines 1))

;; Transparently open compressed files
(auto-compression-mode t)

;; Save a list of recent files visited.
(recentf-mode 1)

;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1)

;; enable column-number-mode
(column-number-mode t)

;; enable narrow view
(put 'narrow-to-region 'disabled nil)

;; Highlight tabulations
(setq-default highlight-tabs t)

;; Show trailing white spaces
(setq-default show-trailing-whitespace t)

(set-default 'indent-tabs-mode nil)
(set-default 'indicate-empty-lines t)
(set-default 'imenu-auto-rescan t)

;; Ido
(ido-mode t)
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-max-prospects 10)

;; YASnippet
(require 'yasnippet)
(yas-global-mode 1)

;; Uniquify
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(defalias 'yes-or-no-p 'y-or-n-p)

(random t) ;; Seed the random-number generator

;; Don't clutter up directories with files~
(defconst emacs-tmp-dir (format "%s/%s%s/" temporary-file-directory "emacs" (user-uid)))
(setq backup-directory-alist
      `((".*" . ,emacs-tmp-dir)))
(setq auto-save-file-name-transforms
      `((".*" ,emacs-tmp-dir t)))
(setq auto-save-list-file-prefix
      emacs-tmp-dir)


(setq backup-directory-alist
      `((".*" . ,(expand-file-name (concat dotfiles-dir "backups")))))
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name (concat dotfiles-dir "backups")))))

(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))

;; Savehist
(setq savehist-file "~/.emacs.d/savehist")
(savehist-mode 1)
(setq history-length t)
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history 1)
(setq savehist-additional-variables
      '(kill-ring
        search-ring
        regexp-search-ring))

;; Default to unified diffs
(setq diff-switches "-u -w")

;; ;; PHP-mode
;; (add-to-list 'auto-mode-alist '("\\.php" . php+-mode))

;; Javascript
(add-to-list 'auto-mode-alist '(".js$" . js2-mode))

;; Undo tree
(require 'undo-tree)
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
    ad-do-it))

;; Setup theme after elpa load
(defun my-after-init ()
  (require 'twilight-theme))
(add-hook 'after-init-hook 'my-after-init)

;; Add rbenv to load path
(setenv "PATH" (concat (getenv "HOME") "/.rbenv/shims:" (getenv "HOME")
                       "/.rbenv/bin:" (getenv "PATH")))
(setq exec-path (cons (concat (getenv "HOME") "/.rbenv/shims")
                      (cons (concat (getenv "HOME") "/.rbenv/bin") exec-path)))

;; Flycheck
(add-hook 'find-file-hook 'flycheck-mode)

;; (require 'flycheck-pos-tip)
(with-eval-after-load 'flycheck
  (flycheck-pos-tip-mode))

;; SVN
(autoload 'svn-status "dsvn" "Run `svn status'." t)
(autoload 'svn-update "dsvn" "Run `svn update'." t)
(require 'vc-svn)

;; Smex
;; (global-set-key [(meta x)] (lambda ()
;;                              (interactive)
;;                              (or (boundp 'smex-cache)
;;                                  (smex-initialize))
;;                              (global-set-key [(meta x)] 'smex)
;;                              (smex)))

;; (global-set-key [(shift meta x)] (lambda ()
;;                                    (interactive)
;;                                    (or (boundp 'smex-cache)
;;                                        (smex-initialize))
;;                                    (global-set-key [(shift meta x)] 'smex-major-mode-commands)
;;                                    (smex-major-mode-commands)))

;; (defadvice smex (around space-inserts-hyphen activate compile)
;;   (let ((ido-cannot-complete-command
;;          `(lambda ()
;;             (interactive)
;;             (if (string= " " (this-command-keys))
;;                 (insert ?-)
;;               (funcall ,ido-cannot-complete-command)))))
;;     ad-do-it))

;; helm
(helm-mode 1)

;; projectile
(projectile-global-mode)
(setq projectile-enable-caching t)
(setq projectile-svn-command "/opt/svn-1.6/bin/svn list -R . | grep -v '$/' | tr '\\n' '\\0'")
(global-set-key (kbd "C-c h") 'helm-projectile)

;; perspective
(persp-mode)
(require 'persp-projectile)

;; switch-window
(require 'switch-window)
(winner-mode 1)

(setq scss-compile-at-save nil)

;; electric pair
(electric-pair-mode t)

;; browse-url browser
(setq browse-url-browser-function 'browse-url-chromium)

;;
(require 'key-bindings)
(require 'misc-func)
(require 'setup-web)
(require 'setup-php)
(require 'setup-js)
(require 'setup-clojure)
(require 'setup-term)
(require 'setup-org)
(require 'setup-powerline)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(coffee-tab-width 2)
 '(php+-mode-delete-trailing-whitespace t)
 '(php+-mode-php-compile-on-save nil)
 '(php+-mode-show-project-in-modeline t)
 '(php+-mode-show-trailing-whitespace t)
 '(php-doc-default-author (quote ("Espen Volden" . "voldern@hoeggen.net")))
 '(php-file-patterns (quote ("\\.php[s345t]?\\'" "\\.inc\\'")))
 '(php-html-basic-offset 4)
 '(php-project-list (quote (("direkte" "~/webdev/html/livestudio" "~/webdev/html/livestudio/TAGS" nil "" nil (("" . "") "" "" "" "" "" "" "" "") "Livestudio" ""))))
 '(phpcs-standard "VG"))
