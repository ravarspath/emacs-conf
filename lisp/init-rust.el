
(require 'rust-mode)

(require 'racer)
(require 'flycheck-rust)
(require 'cargo)

(require 'lsp)
(require 'company-lsp)

(push 'company-lsp company-backends)

(add-hook 'rust-mode-hook 'lsp-mode)
(add-hook 'rust-mode-hook 'company-mode)
(add-hook 'rust-mode-hook 'auto-revert-mode)
(add-hook 'rust-mode-hook 'electric-pair-mode)
(add-hook 'rust-mode-hook 'cargo-minor-mode)
(add-hook 'rust-mode-hook 'rust-enable-format-on-save)
(add-hook 'racer-mode-hook 'eldoc-mode)
(add-hook 'rust-mode-hook 'racer-mode)
(add-hook 'rust-mode-hook 'flycheck-mode)
(add-hook 'flycheck-mode-hook 'flycheck-rust-setup)

;; (keyboard-translate ?\- ?\-)
;; (keyboard-translate ?\_ ?\_) saving because intersting function
(define-key rust-mode-map (kbd "-") (lambda () (interactive) (insert-char #x5f)))
(define-key rust-mode-map (kbd "_") (lambda () (interactive) (insert-char #x2d)))
(define-key rust-mode-map (kbd "C-c C-a") 'company-mode)
(define-key rust-mode-map (kbd "H-r") 'lsp-rename)
(define-key rust-mode-map (kbd "H-l") 'lsp-avy-lens)
(define-key rust-mode-map (kbd "H-m H-l") 'lsp-lens-mode)

;; is lens mode any good for rust??
(rust-enable-format-on-save)
(setq rust-match-angle-brackets nil)
(setq exec-path(append exec-path '("~/.cargo/bin")))
(setq racer-cmd "~/.cargo/bin/racer")
(setq racer-rust-src-path "~/code/rust/src/")

(provide 'init-rust)
