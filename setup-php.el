;;; setup-php.el --- PHP setup / options

;;; Commentary:

;;; Code:
(eval-when-compile
  (require 'flycheck)
  (require 'php-electric))

(defun setup-multi-web-mode ()
  "Function to setup multi-web-mode."
  (require 'php-mode)
  (require 'php+-mode)
  (php+-mode-setup)

  (require 'multi-web-mode)
  (setq mweb-default-major-mode 'html-mode)
  (setq mweb-tags '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
                    (js-mode "<script +\\(type=\"text/javascript\"\\|language=\"javascript\"\\)[^>]*>" "</script>")
                    (css-mode "<style +type=\"text/css\"[^>]*>" "</style>")))
  (setq mweb-filename-extensions '("htm" "html" "ctp" "phtml"))
  (multi-web-global-mode 1))

(add-hook 'after-init-hook 'setup-multi-web-mode)

(add-hook 'php+-mode-hook (lambda ()
                            (require 'php-electric)
                            (php-electric-mode)
                            (setq-local flycheck-phpcs-standard "VG")
                            (setq-local flycheck-phpmd-rulesets '("codesize" "design" "naming" "unusedcode"))
                            (flycheck-mode 1)))

(flycheck-define-checker flycheck-checker-php+
  "PHP+ mode flycheck"
  :command ("php" "-l" "-d" "error_reporting=E_ALL" "-d" "display_errors=1" "-d" "log_errors=0" source)
  :error-patterns ((error "\\(?:Parse\\|Fatal\\|syntax\\) error[:,] \\(.*\\) in \\(.*\\) on line \\([0-9]+\\)"))
  :modes php+-mode)

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

(setq phpcs-standard "VG")

;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(php+-mode-delete-trailing-whitespace t)
;;  '(php+-mode-php-compile-on-save t)
;;  '(php+-mode-show-project-in-modeline t)
;;  '(php+-mode-show-trailing-whitespace t)
;;  '(php-doc-default-author (quote ("Espen Volden" . "voldern@hoeggen.net")))
;;  '(php-file-patterns (quote ("\\.php[s345t]?\\'" "\\.inc\\'")))
;;  '(php-html-basic-offset 4)
;;  '(php-project-list (quote (("direkte" "~/webdev/html/livestudio" "~/webdev/html/livestudio/TAGS" nil "" nil (("" . "") "" "" "" "" "" "" "" "") "Livestudio" "")))))

(add-hook 'html-mode-hook
          (lambda ()
            (set (make-local-variable 'sgml-basic-offset) 4)))

(provide 'setup-php)
;;; setup-php.el ends here
