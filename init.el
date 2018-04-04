;;; init.el --- Initial emacs configuration file
;;; Commentary:
;;; Code:

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(require 'cask "/usr/share/emacs/site-lisp/cask/cask.el")
(cask-initialize)

(require 'req-package)

;; Load configurations
(defconst dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))
(add-to-list 'load-path (concat dotfiles-dir "lisp"))

(setq custom-file (concat dotfiles-dir "custom.el"))
(load custom-file)

(require 'setup-general)
(require 'setup-packages)
(require 'setup-web)
(require 'setup-org)
(require 'setup-rust)

(setq use-package-always-ensure t)

(setq use-package-always-ensure t)

(req-package-finish)

(provide 'init)
;;; init.el ends here
