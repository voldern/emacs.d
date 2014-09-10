;; Load path etc.
(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))

(add-to-list 'load-path dotfiles-dir)
(add-to-list 'load-path "~/.emacs.d/vendor/")
(add-to-list 'load-path "~/.emacs.d/vendor/phpplus-mode")

;; ELPA
(require 'setup-packages)

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

;; Cleanup whitespace before saving
(add-hook 'before-save-hook 'whitespace-cleanup)

;; Ido
(ido-mode t)
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-max-prospects 10)

(set-default 'indent-tabs-mode nil)
(set-default 'indicate-empty-lines t)
(set-default 'imenu-auto-rescan t)

;; YASnippet
(require 'yasnippet)
(yas-global-mode 1)

;; Uniquify
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(defalias 'yes-or-no-p 'y-or-n-p)

(random t) ;; Seed the random-number generator

;; Don't clutter up directories with files~
(setq backup-directory-alist `(("." . ,(expand-file-name
                                        (concat dotfiles-dir "backups")))))

(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))

;; Default to unified diffs
(setq diff-switches "-u -w")

(eval-after-load 'diff-mode
  '(progn
     (set-face-foreground 'diff-added "green4")
     (set-face-foreground 'diff-removed "red3")))

(eval-after-load 'magit
  '(progn
     (set-face-foreground 'magit-diff-add "green3")
     (set-face-foreground 'magit-diff-del "red3")))

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

;; After elpa load
(defun my-after-init ()
  (require 'twilight-theme))
(add-hook 'after-init-hook 'my-after-init)

(font-lock-add-keywords
 'js2-mode `(("\\(function *\\)("
              (0 (progn (compose-region (match-beginning 1)
                                        (match-end 1) "f")
                        nil)))))

;; Add rbenv to load path
(setenv "PATH" (concat (getenv "HOME") "/.rbenv/shims:" (getenv "HOME")
                       "/.rbenv/bin:" (getenv "PATH")))
(setq exec-path (cons (concat (getenv "HOME") "/.rbenv/shims")
                      (cons (concat (getenv "HOME") "/.rbenv/bin") exec-path)))

;; Flycheck
(add-hook 'find-file-hook 'flycheck-mode)

;; SVN
(autoload 'svn-status "dsvn" "Run `svn status'." t)
(autoload 'svn-update "dsvn" "Run `svn update'." t)
(require 'vc-svn)

;; Smex
(global-set-key [(meta x)] (lambda ()
                             (interactive)
                             (or (boundp 'smex-cache)
                                 (smex-initialize))
                             (global-set-key [(meta x)] 'smex)
                             (smex)))

(global-set-key [(shift meta x)] (lambda ()
                                   (interactive)
                                   (or (boundp 'smex-cache)
                                       (smex-initialize))
                                   (global-set-key [(shift meta x)] 'smex-major-mode-commands)
                                   (smex-major-mode-commands)))

(defadvice smex (around space-inserts-hyphen activate compile)
  (let ((ido-cannot-complete-command
         `(lambda ()
            (interactive)
            (if (string= " " (this-command-keys))
                (insert ?-)
              (funcall ,ido-cannot-complete-command)))))
    ad-do-it))

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

;;
(require 'key-bindings)
(require 'misc-func)
(require 'setup-html)
(require 'setup-php)
(require 'setup-js)
(require 'setup-term)

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
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.

 )
