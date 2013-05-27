;;; setup-packages.el --- Packages/elpa setup / options

;;; Commentary:

;;; Code:
(setq package-list '(php-mode multi-web-mode haml-mode yaml-mode magit
                              flymake-php flymake-ruby ace-jump-mode twilight-theme
                              switch-window smex flycheck tagedit))

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages")
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
