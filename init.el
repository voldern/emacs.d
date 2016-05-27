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

(req-package setup-general)

(req-package-finish)

(provide 'init)
;;; init.el ends here
