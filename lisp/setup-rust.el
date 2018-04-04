;;; setup-rust.el --- Rust
;;; Commentary:
;;; Code:
(require 'req-package)

(req-package rust-mode
  :mode "\\.rs\\'")

(req-package racer
  :init
  (unless (getenv "RUST_SRC_PATH")
    (setenv "RUST_SRC_PATH" (expand-file-name "/usr/src/rust/src")))
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode))

(req-package company-racer
  :require racer company
  :config
  (add-to-list 'company-backends 'company-racer))

(provide 'setup-rust)
;;; setup-rust.el ends here
