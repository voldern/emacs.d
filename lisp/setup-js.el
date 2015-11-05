;;; setup-js.el --- Javascript setup / options

;;; Commentary:

;;; Code:
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
          '(javascript-jshint)))

(flycheck-add-mode 'javascript-eslint 'web-mode)

(eval-after-load 'tern
  '(progn
     (require 'tern-auto-complete)
     (tern-ac-setup)))

;;;;; js3-mode ;;;;;
;; (defun after-init-js3-mode ()
;;   "After js3-mode init."
;;   (require 'flycheck)
;;   (require 'auto-complete)
;;   (auto-complete-mode t)
;;   (tern-mode t)
;;   (imenu-add-menubar-index)
;;   (add-hook 'before-save-hook 'whitespace-cleanup nil t))

;; (setq js3-indent-level 4)

;; (add-hook 'js3-mode-hook 'after-init-js3-mode)

;;;;; js2-mode ;;;;;

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-jsx-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-jsx-mode))

;; Turn function() into f() in js2-mode
(font-lock-add-keywords
 'js2-mode `(("\\(function *\\)("
              (0 (progn (compose-region (match-beginning 1)
                                        (match-end 1) "f")
                        nil)))))

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
  (add-hook 'before-save-hook 'whitespace-cleanup nil t)
  (js2r-add-keybindings-with-prefix "C-c C-m"))

(setq js2-global-externs '("define"))
(setq js2-bounce-indent-p t)
(setq js2-mode-indent-ignore-first-tab t)
(setq js2-mode-show-strict-warnings nil)

(add-hook 'js2-mode-hook 'after-init-js2-mode)

;; (require 'skewer-mode)
;; (skewer-setup)

;; (defun skewer-start ()
;;   (interactive)
;;   (let ((httpd-port 8023))
;;     (httpd-start)
;;     (message "Ready to skewer the browser. Now jack in with the bookmarklet.")))

;; (defun skewer-demo ()
;;   (interactive)
;;   (let ((httpd-port 8024))
;;     (run-skewer)
;;     (skewer-repl)))

(provide 'setup-js)
;;; setup-js.el ends here
