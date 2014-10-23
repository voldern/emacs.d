;;; setup-js.el --- Javascript setup / options

;;; Commentary:

;;; Code:

;; Replace inline function returns with <
(font-lock-add-keywords
 'js2-mode `(("function *([^)]*) *{ *\\(return\\) "
              (0 (progn (compose-region (match-beginning 1)
                                        (match-end 1) "<")
                        nil)))))

(defun after-init-js2-mode ()
  "After js2-mode init."
  (require 'flycheck)
  (require 'auto-complete)
  (require 'js2-refactor)
  (auto-complete-mode t)
  (tern-mode t)
  (imenu-add-menubar-index)
  (hs-minor-mode t)
  (js2r-add-keybindings-with-prefix "C-c C-m"))

(setq js2-global-externs '("define"))
(setq js2-bounce-indent-p nil)
(setq js2-mode-indent-ignore-first-tab t)
(setq js2-mode-show-strict-warnings nil)

(add-hook 'js2-mode-hook 'after-init-js2-mode)

(eval-after-load 'tern
  '(progn
     (require 'tern-auto-complete)
     (tern-ac-setup)))

(provide 'setup-js)
;;; setup-js.el ends here
