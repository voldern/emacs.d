;;; setup-php.el --- PHP setup / options

;;; Commentary:

;;; Code:
(eval-when-compile
  (require 'flycheck)
  (require 'php-mode)
  (require 'php-electric))

(defun setup-multi-web-mode ()
  "Function to setup multi-web-mode."
  (require 'php-mode)
  (require 'multi-web-mode)
  (setq mweb-default-major-mode 'html-mode)
  (setq mweb-tags '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
                    (js-mode "<script\\( +\\(type=\"text/javascript\"\\|language=\"javascript\"\\)\\)?[^>]*>" "</script>")
                    (css-mode "<style\\( +\\type=\"text/css\"\\)?[^>]*>" "</style>")))
  (setq mweb-filename-extensions '("htm" "html" "ctp" "phtml"))
  (multi-web-global-mode 1))

(add-hook 'after-init-hook 'setup-multi-web-mode)

(add-hook 'php-mode-hook (lambda ()
                           (require 'php-electric)
                           (php-electric-mode)
                           (setq-local flycheck-phpcs-standard "PSR2")
                           (setq-local flycheck-phpmd-rulesets '("codesize" "design" "naming" "unusedcode"))
                           (setq php-template-compatibility nil)
                           (subword-mode 1)
                           (Flycheck-mode 1)))

(flycheck-define-checker flycheck-checker-php
  "PHP mode flycheck"
  :command ("php" "-l" "-d" "error_reporting=E_ALL" "-d" "display_errors=1" "-d" "log_errors=0" source)
  :error-patterns ((error "\\(?:Parse\\|Fatal\\|syntax\\) error[:,] \\(.*\\) in \\(.*\\) on line \\([0-9]+\\)"))
  :modes php-mode)

(defun my-after-init-php ()
  "After php init hook."
  (require 'flycheck))

(add-hook 'after-init-hook 'my-after-init-php)

(defun bury-compile-buffer-if-successful (buffer string)
  "Bury a compilation buffer if succeeded without warnings"
  (if (and
       (string-match "compilation" (buffer-name buffer))
       (string-match "finished" string)
       (not
        (with-current-buffer buffer
          (search-forward "warning" nil t))))
      (run-with-timer 0.01 nil
                      (lambda (buf)
                        (bury-buffer buf)
                        (delete-window (get-buffer-window buf))
                        (kill-buffer buf)
                        ;; (shell-command "growlnotify -m 'Success' -t 'PHP Compilation' --appIcon 'Emacs' &> /dev/null")
                        )
                      buffer)))

(add-hook 'compilation-finish-functions 'bury-compile-buffer-if-successful)

(setq phpcs-standard "PSR2")

;; (font-lock-remove-keywords
;;  'php-mode `(("function .*(.*)\\(\n *{\\)\n"
;;               (0 (progn (compose-region (match-beginning 1)
;;                                         (match-end 1) "
;;     {")
;;                         nil)))))

(font-lock-add-keywords
 'php-mode `(("function .*(.*)\\(\n *{\\)\n"
              (0 (progn (put-text-property (match-beginning 1)
                                           (match-end 1) 'display " ❴")
                        nil)))
             ("class .*\\(\n *{\\)\n"
              (0 (progn (put-text-property (match-beginning 1)
                                           (match-end 1) 'display " ❴")
                        nil)))))

(add-hook 'html-mode-hook
          (lambda ()
            (set (make-local-variable 'sgml-basic-offset) 4)))

(provide 'setup-php)
;; setup-php.el ends here
