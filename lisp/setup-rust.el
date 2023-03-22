;;; setup-rust.el --- Rust
;;; Commentary:
;;; Code:

(use-package rustic
  :straight t
  :ensure
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status))
  :config
  (setq rustic-format-on-save t)
  (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook))

(defun rk/rustic-mode-hook ()
  ;; so that run C-c C-c C-r works without having to confirm, but don't try to
  ;; save rust buffers that are not file visiting. Once
  ;; https://github.com/brotzeit/rustic/issues/253 has been resolved this should
  ;; no longer be necessary.
  (when buffer-file-name
    (setq-local buffer-save-without-query t))
  (add-hook 'before-save-hook 'lsp-format-buffer nil t))

;; (use-package rust-mode
;;   :mode "\\.rs\\'")

;; (use-package racer
;;   :hook ((rust-mode . racer-mode) (racer-mode . eldoc-mode))
;;   :init
;;   ;; (unless (getenv "RUST_SRC_PATH")
;;   ;;   (setenv "RUST_SRC_PATH" (expand-file-name "/usr/src/rust/src")))
;;   )
;; (use-package company-racer
;;   :config
;;   (add-to-list 'company-backends 'company-racer))

(provide 'setup-rust)
;;; setup-rust.el ends here
