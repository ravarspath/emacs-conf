
;; (maybe-require-package 'rust-mode)
(maybe-require-package 'rust)

(maybe-require-package 'racer)
(maybe-require-package 'flycheck-rust)
(maybe-require-package 'cargo)

(maybe-require-package 'lsp-mode)
;; (setq lsp-auto-configure nil)

 ;; yas-snippet config
(setq-local yas-inhibit-overlay-modification-protection t)

;; (maybe-require-package 'company-lsp)
(maybe-require-package 'toml-mode)

;; (require 'company-lsp)
;; (push 'company-lsp company-backends)

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
(use-package rust-mode
  :bind (:map rust-mode-map
	      ("-" . (lambda () (interactive) (insert-char #x5f)))
	      ("_" . (lambda () (interactive) (insert-char #x2d)))
	      ("C-c C-a" . company-mode)
	      ("C-." . lsp-find-definition)))
  ;;doesn't play nice hyper key
  ;; :config (progn
  ;; 	    ;; (define-key rust-mode-map (kbd "H-r") lsp-rename)
  ;; 	    (define-key rust-mode-map (kbd "H-l") lsp-avy-lens)
  ;; 	    (define-key rust-mode-map (kbd "H-m H-l") lsp-lens-mode)))

(setq rust-format-one-save t)
;; is lens mode any good for rust??
(setq rust-match-angle-brackets nil)
(setq exec-path(append exec-path '("~/.cargo/bin")))
(setq racer-cmd "~/.cargo/bin/racer")
(setq racer-rust-src-path "/usr/lib/rustlib/src/rust/library")

;;lsp has too many anti features
(setq lsp-headerline-breadcrumb-enable nil)
(setq lsp-modeline-diagnostics-enable nil)
(setq lsp-signature-auto-activate nil)

(provide 'init-rust)
