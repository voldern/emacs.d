(require 'cider)

(add-hook 'clojure-mode-hook 'turn-on-eldoc-mode)

(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)

(setq cider-repl-use-clojure-font-lock t)

(setq cider-popup-stacktraces nil)

(setq cider-prefer-local-resources t)

;; Make C-c C-z switch to the CIDER REPL buffer in the current window:
(setq cider-repl-display-in-current-window t)

(setq cider-repl-result-prefix ";; => ")

(setq cider-interactive-eval-result-prefix ";; => ")

(setq cider-repl-wrap-history t)
(setq cider-repl-history-size 1000) ; the default is 500

(add-hook 'cider-repl-mode-hook 'paredit-mode)

(add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)

(defvar ha-4clojure-place-file (concat user-emacs-directory "4clojure-place.txt"))

(defadvice 4clojure-open-question (around 4clojure-open-question-around)
  "Start a cider/nREPL connection if one hasn't already been started when
opening 4clojure questions"
  ad-do-it
  (unless cider-current-clojure-buffer
    (cider-jack-in)))

(provide 'setup-clojure)
