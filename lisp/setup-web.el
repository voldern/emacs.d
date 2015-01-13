(eval-after-load "sgml-mode"
  '(progn
     (require 'tagedit)
     (define-key html-mode-map (kbd "C-c C-w") 'html-wrap-in-tag)
     (tagedit-add-experimental-features)
     (add-hook 'html-mode-hook (lambda () (tagedit-mode 1)))))

(setq sgml-basic-offset 4)

(defun syntax-color-hex ()
  "Syntax color hex color spec such as 「#ff1100」 in current buffer."
  (interactive)
  (font-lock-add-keywords
   nil
   '(("\\(#\\)[abcdef[:digit:]]\\{6\\}"
      (0 (put-text-property
          (match-beginning 1)
          (match-end 1)
          'face (list :background (match-string-no-properties 0)))))))
  (font-lock-fontify-buffer))

(add-hook 'css-mode-hook 'syntax-color-hex)
(add-hook 'scss-mode-hook 'syntax-color-hex)

(provide 'setup-web)
