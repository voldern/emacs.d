;;; setup-js.el --- Javascript setup / options

;;; Commentary:

;;; Code:
(flycheck-declare-checker flycheck-checker-jslint
  "jslint checker"
  :command '("jsl" "-process" source)
  :error-patterns '(("^\\(.+\\)\:\\([0-9]+\\)\: \\(SyntaxError\:.+\\)\:$" error)
                    ("^\\(.+\\)(\\([0-9]+\\)): \\(SyntaxError:.+\\)$" error)
                    ("^\\(.+\\)(\\([0-9]+\\)): \\(lint \\)?\\(warning:.+\\)$" warning)
                    ("^\\(.+\\)\:\\([0-9]+\\)\: strict \\(warning: trailing comma.+\\)\:$" warning))
  :modes 'js2-mode)

(defun my-after-init-js ()
  "After js init hook."
  (require 'flycheck)
  (add-to-list 'flycheck-checkers 'flycheck-checker-jslint))
(add-hook 'after-init-hook 'my-after-init-js)

(defun after-init-js2-mode ()
  "After js2-mode init."
  (auto-complete-mode t)
  (tern-mode t)
  (imenu-add-menubar-index)
  (hs-minor-mode t))

(add-hook 'js2-mode-hook 'after-init-js2-mode)

(eval-after-load 'tern
  '(progn
     (require 'tern-auto-complete)
     (tern-ac-setup)))

(provide 'setup-js)
;;; setup-js.el ends here
