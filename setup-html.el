(eval-after-load "sgml-mode"
  '(progn
     (require 'tagedit)
     (define-key html-mode-map (kbd "C-c C-w") 'html-wrap-in-tag)
     (tagedit-add-experimental-features)
     (add-hook 'html-mode-hook (lambda () (tagedit-mode 1)))))

(provide 'setup-html)
