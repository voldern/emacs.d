;;; setup-general.el --- General configuration
;;; Commentary:
;;; Code:
(defvar dotfiles-dir)

;;; General
;; Font
(when (member "Envy Code R" (font-family-list))
  (add-to-list 'default-frame-alist '(font .  "Envy Code R-12")))

;; UTF-8 all the things
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Translate escape sequences
(ansi-color-for-comint-mode-on)

;; Disable startup screen
(setq inhibit-startup-message t)

;; Enable mouse in xterm compatible terminals
(setq xterm-mouse-mode t)

;; Disable menu
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Transparently open compressed files
(setq auto-compression-mode t)

;; Save a list of recent files visited.
(setq recentf-mode 1)

;; Highlight matching parentheses when the point is on them.
(setq show-paren-mode 1)

;; Enable column-number-mode
(setq column-number-mode t)

;; Highlight tabulations
(setq-default highlight-tabs t)

;; Show trailing white spaces
(setq-default show-trailing-whitespace t)

;; Use spaces instead of tabs
(set-default 'indent-tabs-mode nil)

;; Indicate empty lines
(set-default 'indicate-empty-lines t)

;; Set tab width to 4 by default
(set-default 'tab-width 4)

;; Default to unified diffs
(setq diff-switches "-u -w")

;; Alias y and n to yes or no
(defalias 'yes-or-no-p 'y-or-n-p)

;; Seed the random-number generator
(random t)

;; Do not clutter directories with temporary files
(defconst emacs-tmp-dir (format "%s/%s%s/" temporary-file-directory "emacs" (user-uid)))
(setq backup-directory-alist
      `((".*" . ,emacs-tmp-dir)))
(setq auto-save-file-name-transforms
      `((".*" ,emacs-tmp-dir t)))
(setq auto-save-list-file-prefix
      emacs-tmp-dir)

;; Use chrome as the default browser
(setq browse-url-browser-function 'browse-url-chromium)

;; Enable builtin modes
(electric-pair-mode t)
(winner-mode t)

;;; Key bindings
;; Font size
(bind-key "C-+" 'text-scale-increase)
(bind-key "C--" 'text-scale-decrease)

;; Use regex searches by default.
(bind-key "C-s" 'isearch-forward-regexp)
(bind-key "\C-r" 'isearch-backward-regexp)
(bind-key "C-M-s" 'isearch-forward)
(bind-key "C-M-r" 'isearch-backward)

;; Newline and indent
(bind-key "C-j" 'newline-and-indent)

;; Use shift+direction to move between windows
(windmove-default-keybindings) ;; Shift+direction

(provide 'setup-general)
;;; setup-general.el ends here