;;; init.el --- Initial emacs configuration file
;;; Commentary:
;;; Code:
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

(setq use-package-always-ensure t)

(req-package-finish)

(provide 'init)
;;; init.el ends here
