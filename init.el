;; "Inspired heavily" by https://github.com/purcell/emacs.d/blob/master/init.el

;;--------------------------------------------------------------------------------
;; From here to close all comes from purcell

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(setq normal-gc-cons-threshold (* 20 1024 1024))
(let ((init-gc-cons-threshold (* 1024 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
           (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

(require 'init-utils)
(require 'init-site-lisp) ;; Must come before elpa, as it may provide package.el
;; Calls (package-initialize)
(require 'init-elpa)      ;; Machinery for installing required packages
(require 'init-exec-path) ;; Set up $PATH

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;;--------------------------------------------------------------------------------

;;--------------------------------------------------------------------------------
;; package "agnostic" configs
;;--------------------------------------------------------------------------------
(electric-pair-mode 1)
(which-key-mode 1)

(global-linum-mode)
(defun buffer-too-big-p ()
  (or (> (buffer-size) (* 5000 80))
      (> (line-number-at-pos (point-max)) 5000)))
(add-hook 'prog-mode-hook
          (lambda ()
            ;; turn off `linum-mode' when there are more than 5000 lines
            (if (buffer-too-big-p) (linum-mode -1))))
(linum-mode t)

(load-theme 'monokai t)
;;super bright, but does handle org
;; (load-theme 'leuven t)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((ipython . t)
   (clojure . t)
   (J . t)
   ))

(desktop-save-mode 1)
(auto-revert-mode)

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(fset 'yes-or-no-p 'y-or-n-p)
(show-paren-mode 1)

;;makes the init file prettier
(font-lock-add-keywords 'emacs-lisp-mode '(("(\\(maybe-require-package\\)\\_>[ 	']*\\(\\(?:\\sw\\|\\s_\\|\\\\.\\)+\\)?"
			       (1 font-lock-keyword-face)
			       (2 font-lock-constant-face ))))

(set-face-attribute 'default nil :height 85)
(global-set-key (kbd "H--") 'text-scale-adjust)
(global-set-key (kbd "H-=") 'text-scale-adjust)

;;--------------------------------------------------------------------------------
;; package soup
;;--------------------------------------------------------------------------------

(require 'key-chord)
(setq key-chord-two-keys-delay 0.2)
(require 'dap-gdb-lldb)
(dap-gdb-lldb-setup)
(require 'company)
(require 'flycheck)
(require 'eldoc)

(require 'init-rust)

;; (use-package lsp-mode
;;   :commands lsp
;;   :config (require 'lsp-clients))
;; (use-package lsp-ui)
;; don't like lsp, its too clever by half
;; warning about ptrace scope you unhardend this

(setq backup-directory-alist '(("" . "~/.emacs.d/backup")))

(add-to-list 'load-path "~/software/mu/mu4e")
(require 'mu4e)
(setq mu4e-maildir "~/maildir")
(define-key mu4e-view-mode-map (kbd "C-o") 'org-open-at-point)
(require 'mu4e-contrib)
(setq mu4e-html2text-command 'mu4e-shr2text)
(setq shr-color-visible-luminance-min 60)
(setq shr-color-visible-distance-min 5)
(setq shr-use-colors nil)
(advice-add #'shr-colorize-region :around (defun shr-no-colourise-region (&rest ignore)))
(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))

(add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)
(setq org-confirm-babel-evaluate nil)
(define-key org-mode-map (kbd "H-[") 'org-agenda)
;;latex drives me up a wall
(key-chord-define org-mode-map "qi" '(lambda () (interactive) (insert "\\")))

(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)
(setq jedi:setup-keys t)
;;(add-hook 'python-mode-hook 'subword-mode)
;; (setq jedi:environment-root "jedi")  ; or any other name you like
;; (setq jedi:environment-virtualenv
;;       (append python-environment-virtualenv
;;               '("--python" "/usr/bin/python3")))

(setq org-src-window-setup 'other-window)
(setq org-src-preserve-indentation t)

;;(add-hook 'org-mode-hook 'auto-complete-mode)

(add-hook 'org-src-mode-hook
	  (lambda () (local-set-key (kbd "C-c C-e")
		       'org-src-do-key-sequence-at-code-block)))

(add-hook 'python-mode-hook 'hs-minor-mode)
(require 'circe)
(define-key circe-mode-map (kbd "M-n") "\C-u4\C-v")
(define-key circe-mode-map (kbd "M-p") "\C-u4\M-v")
(require 'man)
(define-key Man-mode-map (kbd "M-n") "\C-u4\C-v")
(define-key Man-mode-map (kbd "M-p") "\C-u4\M-v")

(eval-after-load "zotxt"
  '(setq zotxt-default-bibliography-style "mkbehr-short"))
(add-hook 'org-mode-hook 'org-zotxt-mode)

(add-to-list 'load-path "~/.emacs.d/customModes/")
(add-to-list 'load-path "~/.emacs.d/addedPackages/")

(add-to-list 'load-path "~/.emacs.d/addedPackages/dired-hacks/")
(add-to-list 'load-path "~/.emacs.d/addedPackages/i3-emacs/")
(require 'dired-subtree)

(require 'latex)
;; (require 'org-define-mode)
;; (add-hook 'org-mode-hook 'org-define-mode)
;; (require 'org-autobuild)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq TeX-save-query nil)
(setq-default TeX-master nil)
(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)

(require 'flymake)
(add-hook 'LaTeX-mode-hook 'flymake-mode)
(define-key flymake-mode-map (kbd "C-c C-,") 'flymake-goto-next-error)
(define-key flymake-mode-map (kbd "C-c C-.") 'flymake-goto-prev-error)
;; (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
;; (setq reftex-plug-into-AUCTeX t)
;; setup for reftex to handle refrences
;; (require 'tex)
(setq TeX-PDF-mode nil)
;;not sure what this does when true, except breaks preview, unless fix from reddit
;;also may want to use synctex? not sure what it does
;;also what the hell is latex-magic buffer

(define-key dired-mode-map "i" 'dired-subtree-insert)
(define-key dired-mode-map ";" 'dired-subtree-remove)

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(setq web-mode-engines-alist
      '(("django"    . "\\.html\\'"))
)

(require 'dired-x)
(add-hook 'dired-load-hook
	  (function (lambda () (load "dired-x"))))

(add-hook 'dired-mode-hook (lambda () (dired-omit-mode)))

(require 'flyspell)
(add-hook 'markdown-mode-hook 'flyspell-mode)
(define-key flyspell-mode-map (kbd "C-c C-d") 'ispell-word)

(require 'i3-integration)
(i3-one-window-per-frame-mode-on)
;; (i3-advice-visible-frame-list-on)

(require 'pdf-tools)
(define-key pdf-view-mode-map (kbd "H") 'pdf-annot-add-highlight-markup-annotation)
(define-key pdf-view-mode-map (kbd "t") 'pdf-annot-add-text-annotation)
(define-key pdf-view-mode-map (kbd "d") 'pdf-annot-delete)
(define-key pdf-view-mode-map (kbd "M-SPC") 'pdf-view-scroll-down-or-previous-page)
(pdf-loader-install)
(add-hook 'pdf-view-mode-hook (lambda () (linum-mode 0)))

(require 'keyfreq)
(keyfreq-mode 1)
(keyfreq-autosave-mode 1)

(eval-when-compile
  (require 'cl))

(setq helm-command-prefix-key "<C-return>")
(require 'helm-config)
(helm-mode 1)
(define-key helm-map (kbd "C-z")  'helm-select-action)
(define-key helm-find-files-map (kbd "C-<backspace>") 'backward-kill-word)
(define-key helm-read-file-map (kbd "C-<backspace>") 'backward-kill-word)
(define-key helm-find-files-map (kbd "<tab>") 'ignore ) 
(define-key helm-read-file-map (kbd "<tab>") 'ignore)
(require 'helm-descbinds)
(helm-descbinds-mode)
(require 'helm-dictionary)
(define-key helm-command-map (kbd "d") 'helm-dictionary)
(define-key helm-command-map (kbd "C-s") 'helm-occur)
(define-key helm-command-map (kbd "C-g") 'helm-grep-do-git-grep)
;;TODO integrate helm with helpful??

(require 'use-package)


(require 'markdown-mode)
(add-hook 'markdown-mode-hook 'visual-line-mode)
(define-key markdown-mode-map "\M-n" "\C-u4\C-v")
(define-key markdown-mode-map "\M-p" "\C-u4\M-v")

;; honestly not that useful
;; (require 'god-mode)
;; (global-set-key (kbd "<escape>") 'god-local-mode)
;; (define-key god-local-mode-map "i" 'god-local-mode)
;; (define-key god-local-mode-map "z" 'repeat)

(use-package avy
  :load-path "~/Desktop/code/custemacs/avy"
  )


(define-key org-mode-map (kbd "C-j") 'avy-goto-word-or-subword-1)
(define-key LaTeX-mode-map (kbd "C-j") 'avy-goto-word-or-subword-1)
(define-key flyspell-mode-map (kbd "C-;") 'avy-goto-char)
(define-key lisp-interaction-mode-map (kbd "C-j") 'avy-goto-word-or-subword-1)
(define-key lisp-interaction-mode-map (kbd "C-M-j") 'eval-print-last-sexp)

(define-key org-mode-map (kbd "H-o") 'helm-occur)
(define-key org-mode-map (kbd "H-l") 'rorg-double-link)
;; (pyvenv-activate "~/envs/benv")


;; (add-hook 'after-init-hook 'global-flycheck-mode)
;; flycheck is nice, but really agressive, I don't want it for hacks
;;hacks like .emacs files


(use-package projectile
  :ensure t
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1))


(calendar)
(define-key calendar-mode-map "\M-[" 'calendar-backward-month)
(define-key calendar-mode-map "\M-]" 'calendar-forward-month)
(appt-activate 1)
(define-key calendar-mode-map "j" 'calendar-backward-day)
(define-key calendar-mode-map "k" 'calendar-forward-week)
(define-key calendar-mode-map "l" 'calendar-backward-week)
(define-key calendar-mode-map ";" 'calendar-forward-day)

(require 'pyim)
(require 'pyim-basedict)
(pyim-basedict-enable)

(add-hook 'python-mode-hook 'blacken-mode)
(define-key python-mode-map (kbd "-") (lambda () (interactive) (insert-char #x5f)))
(define-key python-mode-map (kbd "_") (lambda () (interactive) (insert-char #x2d)))

(require 'ob-clojure)
(setq org-babel-clojure-backend 'cider)
(require 'cider)
(require 'paredit)
(add-hook 'clojure-mode-hook 'enable-paredit-mode)
(define-key paredit-mode-map (kbd "C-j") nil)
(define-key cider-mode-map (kbd "C-c C-e") nil)

(global-hl-todo-mode 1)
(define-key hl-todo-mode-map (kbd "C-c C-h n") 'hl-todo-next)
(define-key hl-todo-mode-map (kbd "C-c C-h p") 'hl-todo-previous)

;; funny but not helpful
;; (require 'disable-mouse)
;; (global-disable-mouse-mode)

(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

(set-fontset-font (frame-parameter nil 'font) 'han "xft:-GOOG-Noto Sans CJK KR-normal-normal-normal-*-16-*-*-*-*-0-iso10646-1")

(use-package nov
  :bind (:map nov-mode-map
	      ("C-d" . youdao-dictionary-search-at-point)
	      ("d" . youdao-dictionary-search-at-point)
	      ("j" . backward-char)
	      (";" . forward-char)
	      ("k" . next-line)
	      ("l" . previous-line)
	      ("g" . keyboard-quit)
	      ("n" . nov-scroll-up)
	      ("p" . nov-scroll-down)
	      ("SPC" . set-mark-command)
	      ("q" . nil )
	      ("C-q" . quit-window)))


(add-to-list 'load-path "~/.emacs.d/addedPackages/Emacs-langtool")
(setq langtool-language-tool-jar "~/software/LanguageTool-4.6/languagetool-commandline.jar")
(require 'langtool)
(define-key markdown-mode-map (kbd "C-c C-v C-c") 'langtool-check)
(define-key markdown-mode-map (kbd "C-c C-v C-d") 'langtool-check-done)

;; (add-to-list 'load-path "~/.emacs.d/addedPackages/ibus-el-0.3.2")
;; (require 'ibus)

(require 'elfeed)
(define-key elfeed-show-mode-map (kbd "<return>") 'shr-copy-url)
(define-key elfeed-search-mode-map (kbd "C-l") 'elfeed-update)
(define-key elfeed-search-mode-map (kbd "k") 'next-line)
(define-key elfeed-search-mode-map (kbd "l") 'previous-line)
;; press S too change filter

(require 'tabbar)
(add-hook 'tabbar-mode-hook (lambda () (interactive) (remove-hook 'kill-buffer-hook 'tabbar-buffer-track-killed)))
(remove-hook 'kill-buffer-hook 'tabbar-buffer-track-killed)

(require 'youdao-dictionary)

(add-to-list 'load-path "~/.emacs.d/addedPackages/q4")
(require 'q4)
(q4/toggle-thumbnailing-method)
(define-key q4-mode-map (kbd "f") 'q4/point-to-next-post)
(define-key q4-mode-map (kbd "j") 'q4/point-to-next-post)

;; (add-to-list 'load-path "~/Desktop/code/newsr/")
;; (require 'newsr)
;; (define-key newsr-view-mode-map (kbd "d") 'youdao-dictionary-search-at-point)
;; (define-key newsr-view-mode-map (kbd "o") 'youdao-dictionary-search-at-point)
;; (define-key newsr-view-mode-map  (kbd "SPC") 'set-mark-command)
;; (define-key newsr-view-mode-map (kbd "j") 'backward-char)
;; (define-key newsr-view-mode-map (kbd ";") 'forward-char)
;; (define-key newsr-view-mode-map (kbd "k") 'next-line)
;; (define-key newsr-view-mode-map (kbd "l") 'previous-line)
;; (define-key newsr-view-mode-map (kbd "g") 'keyboard-quit)

;; (require 'general)

(yas-global-mode)

(require 'diminish)
(diminish 'eldoc-mode)
(diminish 'racer-mode)
(diminish 'disable-mouse-mode)
(diminish 'disable-mouse-global-mode)
(diminish 'projectile-mode)
(diminish 'helm-mode)
(diminish 'yas-minor-mode)
(diminish 'which-key-mode)
(diminish 'rust-mode )

(add-hook 'diary-list-entries-hook 'diary-sort-entries)
(put 'set-goal-column 'disabled nil)


;;--------------------------------------------------------------------------------
;; global binds
;;--------------------------------------------------------------------------------
(global-set-key (kbd "H-d") 'delete-this-file)
(global-set-key (kbd "H-r") 'rename-this-file-and-buffer)

(global-set-key "\M-n" "\C-u4\C-v")
(global-set-key "\M-p" "\C-u4\M-v")

(maybe-require-package 'multiple-cursors)
(global-set-key (kbd "C-c m c") 'mc/edit-lines)
(global-set-key (kbd "<M-right>") 'mc/mark-next-like-this)
(global-set-key (kbd "<M-left>") 'mc/mark-previous-like-this)

(global-set-key [f10] 'magit-status)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key [f1] 'ansi-term)

(global-set-key "\C-x\C-f" 'helm-find-files)
(global-set-key (kbd "C-x k") (lambda () (interactive) (kill-buffer nil)))
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-c C-s") 'helm-occur)
(global-set-key (kbd "H-i") 'helm-imenu)

(global-set-key (kbd "C-M-j") "\C-\M-u\C-\M-f")
(global-set-key (kbd "C-M-n") 'forward-list)
(global-set-key (kbd "M-s") 'paredit-splice-sexp)

(global-set-key (kbd "C-;") 'avy-goto-char)
(global-set-key (kbd "C-j") 'avy-goto-word-or-subword-1)
(global-set-key (kbd "M-j") 'avy-goto-line)

(autoload 'bm-toggle   "bm" "Toggle bookmark in current buffer." t)
(autoload 'bm-next     "bm" "Goto bookmark."                     t)
(autoload 'bm-previous "bm" "Goto previous bookmark."            t)

(global-set-key (kbd "C-c C-b C-t") 'bm-toggle)
(global-set-key (kbd "C-c C-b C-n") 'bm-next)
(global-set-key (kbd "C-c C-b C-p") 'bm-previous)

(require 'caps-lock)
(global-set-key (kbd "M-c") 'caps-lock-mode)

(global-set-key (kbd "C-s-;") 'tabbar-forward-tab)
(global-set-key (kbd "C-s-j") 'tabbar-backward-tab)

(global-set-key (kbd "H-0") 'q4/browse-board)

;;--------------------------------------------------------------------------------
;; Custom functionality that have implemented
;;--------------------------------------------------------------------------------

(require 'ravar-custom)

(global-set-key (kbd "C-<f2>") 'multi-occur-in-this-mode)
(global-set-key [f6] 'revert-this-buffer)
(define-key elfeed-show-mode-map (kbd "o") 'ravar/elfeed-play-enclosure-mpd)
(define-key q4-mode-map (kbd "s") 'q4-override/save-post-image)
(global-set-key (kbd "M-J") 'move-temp-del)
(define-key youdao-dictionary-mode-map (kbd "i") 'youdao-extract)
(global-set-key (kbd "H-c") 'collapse-head-whitespace)
(define-key org-mode-map (kbd "H-k") 'avy-jump-open)
(global-set-key (kbd "H-y" ) 'avy-yank-line)

;;--------------------------------------------------------------------------------
;; Many default configurations are frustrating, loads some modified functions
;;--------------------------------------------------------------------------------
(require 'patches)

;;--------------------------------------------------------------------------------
;; Ensures a running emacsclient
;;--------------------------------------------------------------------------------
(add-hook 'after-init-hook
          (lambda ()
            (require 'server)
            (unless (server-running-p)
              (server-start))))

;;--------------------------------------------------------------------------------
;; Loads configuration from using the 'customize' interface
;;--------------------------------------------------------------------------------
(when (file-exists-p custom-file)
  (load custom-file))

;;--------------------------------------------------------------------------------
;; loads my keybinding
;;--------------------------------------------------------------------------------

(provide 'init)
