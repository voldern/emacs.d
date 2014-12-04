;;; setup-packages.el --- Packages/elpa setup / options

;;; Commentary:

;;; Code:
(setq package-list '(php-mode multi-web-mode haml-mode yaml-mode magit
                              flymake-php flymake-ruby ace-jump-mode twilight-theme
                              undo-tree switch-window flycheck smex tagedit
                              auto-complete js2-mode tern tern-auto-complete ag
                              flx-ido projectile helm helm-ag helm-ag-r helm-projectile
                              perspective persp-projectile yasnippet js2-refactor
                              autopair cider cljdoc rainbow-delimiters flycheck-pos-tip
                              powerline git-gutter-fringe))

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

; activate all the packages (in particular autoloads)
(package-initialize)

; fetch the list of packages available
(when (not package-archive-contents)
  (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (when (not (package-installed-p package))
    (package-install package)))

(provide 'setup-packages)
;;; setup-packages.el ends here