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

(maybe-require-package 'use-package)
(require 'use-package)

;;--------------------------------------------------------------------------------
;; get some packages by git
;;--------------------------------------------------------------------------------
(require 'init-git)


;;--------------------------------------------------------------------------------
;; package "agnostic" configs
;;--------------------------------------------------------------------------------

;;who thought this was a good idea
(setq ring-bell-function 'ignore)

(global-set-key (kbd "H-b") 'switch-to-prev-buffer)
(global-set-key (kbd "H-f") 'switch-to-next-buffer)
(global-set-key (kbd "H-n") 'switch-to-next-buffer)
(global-set-key (kbd "H-p") 'switch-to-prev-buffer)

(electric-pair-mode 1)

(global-linum-mode)
(defun buffer-too-big-p ()
  (or (> (buffer-size) (* 5000 80))
      (> (line-number-at-pos (point-max)) 5000)))
(add-hook 'prog-mode-hook
          (lambda ()
            ;; turn off `linum-mode' when there are more than 5000 lines
            (if (buffer-too-big-p) (linum-mode -1))))
(linum-mode t)

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
;; This has the packages that need to be install by user, or not applicable to
;; everyone, like i3 integration
;;--------------------------------------------------------------------------------
(require 'init-optional)

;;--------------------------------------------------------------------------------
;; package soup
;;-------------------------------------------------------------------------------- 

(maybe-require-package 'unicode-fonts)
(maybe-require-package 'with-editor)
(maybe-require-package 'yasnippet)
(maybe-require-package 'monokai-theme)
(load-theme 'monokai t)
;;super bright, but does handle org
;; (load-theme 'leuven t)

(maybe-require-package 'j-mode)
(maybe-require-package 'which-key)
(which-key-mode 1)

(maybe-require-package 'dash)
(git-ensure-package "https://github.com/Fuco1/dired-hacks.git" "dired-hacks")
(require 'dired-subtree)
(define-key dired-mode-map "i" 'dired-subtree-insert)
(define-key dired-mode-map ";" 'dired-subtree-remove)

(maybe-require-package 'key-chord)
(setq key-chord-two-keys-delay 0.2)
(maybe-require-package 'dap-mode)
(maybe-require-package 'company)
(maybe-require-package 'flycheck)
(require 'eldoc)

(git-ensure-package "https://depp.brause.cc/chuck-mode.git" "chuck-mode")
(require 'chuck-mode)

;;https://github.com/emacsmirror/emacswiki.org/blob/master/isearch-prop.el
;;https://github.com/emacsmirror/emacswiki.org/blob/master/isearch%2b.el
;; (eval-after-load "isearch" '(require 'isearch+))
;; (add-to-list 'load-path (expand-file-name "addedPackages" user-emacs-directory))

;;TODO URGENT fix
(require 'init-rust)

;; (use-package lsp-mode
;;   :commands lsp
;;   :config (require 'lsp-clients))
;; (use-package lsp-ui)
;; don't like lsp, its too clever by half
;; warning about ptrace scope you unhardend this

(setq backup-directory-alist '(("" . "~/.emacs.d/backup")))


;;this is set to make mu4e legible if you ever use it
(setq shr-color-visible-luminance-min 60)
(setq shr-color-visible-distance-min 5)
(setq shr-use-colors nil)
(advice-add #'shr-colorize-region :around (defun shr-no-colourise-region (&rest ignore)))
(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))

(maybe-require-package 'ob-ipython)
(maybe-require-package 'clojure-mode)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((ipython . t)
   (clojure . t)
   (J . t)
   ))

(add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)
(setq org-confirm-babel-evaluate nil)
;; (define-key org-mode-map (kbd "H-[") 'org-agenda)
;;typing latex drives me up a wall
(key-chord-define org-mode-map "qi" '(lambda () (interactive) (insert "\\")))
(key-chord-mode 1)

(maybe-require-package 'jedi)
;; (maybe-require-package 'company-anaconda)
(eval-after-load "company"
  '(add-to-list 'company-backends 'company-jedi))
;; (add-hook 'python-mode-hook 'anaconda-mode)
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
(maybe-require-package 'circe)

(require 'man)
(define-key Man-mode-map (kbd "M-n") "\C-u4\C-v")
(define-key Man-mode-map (kbd "M-p") "\C-u4\M-v")

(maybe-require-package 'zotxt)
(eval-after-load "zotxt"
  '(setq zotxt-default-bibliography-style "mkbehr-short"))
(add-hook 'org-mode-hook 'org-zotxt-mode)

(maybe-require-package 'bash-completion)
(setq bash-completion-nospace t)
(defun bash-completion-eshell-capf ()
  (bash-completion-dynamic-complete-nocomint
   (save-excursion (eshell-bol) (point))
   (point) t))

(defun bash-completion-from-eshell ()
  (interactive)
  (let ((completion-at-point-functions
         '(bash-completion-eshell-capf)))
    (completion-at-point)))
;;WTF
(add-hook 'eshell-mode-hook (lambda () (define-key eshell-mode-map (kbd "C-i") 'bash-completion-from-eshell )))


(maybe-require-package 'flymake)
(add-hook 'LaTeX-mode-hook 'flymake-mode)
(use-package flymake
  :bind (:map flymake-mode-map
	      ("C-c C-," . 'flymake-goto-next-error)
	      ("C-c C-." . 'flymake-goto-prev-error))) 

(maybe-require-package 'auctex)
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

;; (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
;; (setq reftex-plug-into-AUCTeX t)
;; setup for reftex to handle refrences
;; (require 'tex)
(setq TeX-PDF-mode t)
(setq TeX-view-program-selection '((output-pdf "PDF Tools"))
      TeX-source-correlate-start-server t)

(add-hook 'TeX-after-compilation-finished-functions
          #'TeX-revert-document-buffer)

;;TODO use this to use make with auctex
;; (eval-after-load "tex" '(add-to-list 'TeX-command-list
;; 				     '("Make" "make" TeX-run-compile nil t)))
;;(setq latex-run-command "pdflatex")
;; I think uses stock tex instead of auctex

;;not sure what this does when true, except breaks preview, unless fix from reddit
;;also may want to use synctex? not sure what it does
;;also what the hell is latex-magic buffer

(maybe-require-package 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html.tera?\\'" . web-mode))
(setq web-mode-engines-alist
      '(("django"    . "\\.html\\'")
	("django"    . "\\.tera\\'"))
      )
(use-package web-mode
  :bind (:map web-mode-map
	      ("C-c i" . 'web-mode-element-insert)))


(maybe-require-package 'dired-x)
(add-hook 'dired-load-hook
	  (function (lambda () (load "dired-x"))))


(add-hook 'dired-load-hook (lambda () (require 'dired-x)))
(setq dired-omit-mode t)

(maybe-require-package 'flyspell)
(maybe-require-package 'pdf-tools)
(pdf-loader-install)
(add-hook 'pdf-view-mode-hook (lambda () (linum-mode 0)))

(maybe-require-package 'keyfreq)
(keyfreq-mode 1)
(keyfreq-autosave-mode 1)

(eval-when-compile
  (require 'cl))

(setq helm-command-prefix-key "<C-return>")
(maybe-require-package 'helm-config)
(maybe-require-package 'helm-descbinds)
(maybe-require-package 'helm-dictionary)
(maybe-require-package 'helm)
(require 'helm-config)
(helm-mode 1)
(define-key helm-map (kbd "C-z")  'helm-select-action)
(define-key helm-find-files-map (kbd "C-<backspace>") 'backward-kill-word)
(define-key helm-read-file-map (kbd "C-<backspace>") 'backward-kill-word)
(define-key helm-find-files-map (kbd "<tab>") 'ignore ) 
(define-key helm-read-file-map (kbd "<tab>") 'ignore)
(helm-descbinds-mode)
(define-key helm-command-map (kbd "d") 'helm-dictionary)
(define-key helm-command-map (kbd "C-s") 'helm-occur)
(define-key helm-command-map (kbd "C-g") 'helm-grep-do-git-grep)
(add-to-list 'desktop-globals-to-save 'helm-ff-history)
;;TODO integrate helm with helpful??

;;TODO URGENT re-enable
;; (maybe-require-package 'markdown-mode)
;; (add-hook 'markdown-mode-hook 'flyspell-mode)
;; (add-hook 'markdown-mode-hook 'visual-line-mode)
;; (define-key markdown-mode-map "\M-n" "\C-u4\C-v")
;; (define-key markdown-mode-map "\M-p" "\C-u4\M-v")

;; honestly not that useful
;; (require 'god-mode)
;; (global-set-key (kbd "<escape>") 'god-local-mode)
;; (define-key god-local-mode-map "i" 'god-local-mode)
;; (define-key god-local-mode-map "z" 'repeat)



(git-ensure-package "https://github.com/ravarspath/q4" "q4")

(maybe-require-package 'avy)
(define-key org-mode-map (kbd "C-j") 'avy-goto-word-or-subword-1)
(define-key lisp-interaction-mode-map (kbd "C-j") 'avy-goto-word-or-subword-1)
(define-key lisp-interaction-mode-map (kbd "C-M-j") 'eval-print-last-sexp)

(define-key org-mode-map (kbd "H-o") 'helm-occur)
(define-key org-mode-map (kbd "H-l") 'rorg-double-link)

;; (pyvenv-activate "~/envs/benv")

;; (use-package projectile
;;   :ensure t
;;   :config
;;   (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
;;   (projectile-mode +1))


(calendar)
(define-key calendar-mode-map "\M-[" 'calendar-backward-month)
(define-key calendar-mode-map "\M-]" 'calendar-forward-month)
(appt-activate 1)
(define-key calendar-mode-map "j" 'calendar-backward-day)
(define-key calendar-mode-map "k" 'calendar-forward-week)
(define-key calendar-mode-map "l" 'calendar-backward-week)
(define-key calendar-mode-map ";" 'calendar-forward-day)
(define-key calendar-mode-map (kbd "H-i")  'diary-insert-entry)

;; (define-key calendar-mode-map "id"  'diary-insert-entry)
;; (define-key calendar-mode-map "iw"  'diary-insert-weekly-entry)
;; (define-key calendar-mode-map "im"  'diary-insert-monthly-entry)
;; (define-key calendar-mode-map "iy"  'diary-insert-yearly-entry)
;; (define-key calendar-mode-map "ia"  'diary-insert-anniversary-entry)
;; (define-key calendar-mode-map "ib"  'diary-insert-block-entry)
;; (define-key calendar-mode-map "ic"  'diary-insert-cyclic-entry)

(maybe-require-package 'pyim)
(maybe-require-package 'pyim-basedict)
(pyim-basedict-enable)

(maybe-require-package 'pyvenv)
(maybe-require-package 'blacken)

(add-hook 'python-mode-hook 'blacken-mode)
(define-key python-mode-map (kbd "-") (lambda () (interactive) (insert-char #x5f)))
(define-key python-mode-map (kbd "_") (lambda () (interactive) (insert-char #x2d)))
(maybe-require-package 'paredit)
(autoload 'paredit-splice-sexp "paredit")

;; (maybe-require-package 'ob-clojure)
;; (setq org-babel-clojure-backend 'cider)
;; (maybe-require-package 'cider)
;; (add-hook 'clojure-mode-hook 'enable-paredit-mode)
;; (define-key paredit-mode-map (kbd "C-j") nil)
;; (define-key cider-mode-map (kbd "C-c C-e") nil)

(maybe-require-package 'lammps-mode)
(autoload 'lammps-mode "lammps-mode.el" "LAMMPS mode." t)
(setq auto-mode-alist (append auto-mode-alist
                              '(("in\\." . lammps-mode))
                              '(("\\.lmp\\'" . lammps-mode))
                              ))

(maybe-require-package 'hl-todo)
(global-hl-todo-mode 1)
(define-key hl-todo-mode-map (kbd "C-c C-h n") 'hl-todo-next)
(define-key hl-todo-mode-map (kbd "C-c C-h p") 'hl-todo-previous)

;; (maybe-require-package 'yafolding)
;; (define-key yafolding-mode-map (kbd "<C-return>") nil)
;; (define-key yafolding-mode-map (kbd "H-f") 'yafolding-toggle-element)
;; yafolding glitched out hard on me
;; (define-key hs-minor-mode-map (kbd "H-t") 'hs-toggle-hiding)

;; funny but not helpful
;; (require 'disable-mouse)
;; (global-disable-mouse-mode)


(set-fontset-font (frame-parameter nil 'font) 'han "xft:-GOOG-Noto Sans CJK KR-normal-normal-normal-*-16-*-*-*-*-0-iso10646-1")

(git-ensure-package "https://depp.brause.cc/nov.el.git" "nov.el")
;; (setq nov-unzip-program "unzzip") ;;TODO figure out how to unzip
;; (maybe-require-package 'nov)
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
;; (add-to-list 'load-path "~/.emacs.d/addedPackages/ibus-el-0.3.2")
;; (require 'ibus)

(maybe-require-package 'elfeed)
(require 'elfeed)
(define-key elfeed-search-mode-map (kbd "C-l") 'elfeed-update)
(define-key elfeed-search-mode-map (kbd "k") 'next-line)
(define-key elfeed-search-mode-map (kbd "l") 'previous-line)
(define-key elfeed-show-mode-map (kbd "<return>") 'shr-copy-url)

(maybe-require-package 'tabbar)
(add-hook 'tabbar-mode-hook (lambda () (interactive) (remove-hook 'kill-buffer-hook 'tabbar-buffer-track-killed)))
(remove-hook 'kill-buffer-hook 'tabbar-buffer-track-killed)

(maybe-require-package 'youdao-dictionary)

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
;; (define-key yas-minor-mode-map (kbd "C-c C-i ") 'yas-insert-snippet)

(add-hook 'diary-list-entries-hook 'diary-sort-entries)
(put 'set-goal-column 'disabled nil)

(git-ensure-package "https://github.com/ryanswilson59/ob-wolfram" "ob-wolfram")

;; (maybe-require-package 'edbi)



;; TODO get this to run after mode load
(maybe-require-package 'diminish)
(diminish 'eldoc-mode)
(diminish 'racer-mode)
(diminish 'disable-mouse-mode)
(diminish 'disable-mouse-global-mode)
(diminish 'projectile-mode)
(diminish 'helm-mode)
(diminish 'yas-minor-mode)
(diminish 'which-key-mode)
(diminish 'cargo-minor-mode)
(diminish 'company-mode)

(use-package avy
  :bind(( "C-;" . avy-goto-char)
	( "C-j" . avy-goto-word-or-subword-1)
	( "M-j" . avy-goto-line)
	( "M-i" . avy-isearch)))
(global-set-key (kbd "H-m") 'avy-move-region)
(global-set-key (kbd "H-k") 'avy-kill-region)
(global-set-key (kbd "H-y") 'avy-kill-ring-save-region)


;; TODO uncomment figure out what is going on with emacs 28
(use-package latex
  :bind (:map LaTeX-mode-map ("C-j" . avy-goto-word-or-subword-1))
  :bind (:map latex-mode-map ("C-j" . avy-goto-word-or-subword-1))
  :config (key-chord-define LaTeX-mode-map "qi" '(lambda () (interactive) (insert "\\")))
  :after (avy))

(use-package nov
  :bind (:map nov-mode-map
              ("C-d" . youdao-dictionary-search-at-point)
	      ("d" . danish-dictionary-at-point)
	      ("j" . backward-char)
	      (";" . forward-char)
	      ("k" . next-line)
	      ("l" . previous-line)
	      ("g" . keyboard-quit)
	      ("M-n" . scroll-up)
	      ("SPC" . scroll-up)
	      ("n" . "\C-u4\C-v")
	      ("p" . "\C-u4\M-v")
	      ;; ("SPC" . set-mark-command)
	      ("q" . nil )
	      ("C-q" . quit-window)
	      ("s" . nov-save))
  :config (nov-after-load-patch)
  :init
  (add-hook 'nov-post-html-render-hook 'nov-center-text-after-render))
(setq nov-variable-pitch nil)


(use-package flyspell-mode
  :bind (:map flyspell-mode-map
              ("C-c C-d" . ispell-word))
  :after (avy))

(use-package pdf-tools
  :bind (:map pdf-view-mode-map
              ("H" . pdf-annot-add-highlight-markup-annotation)
	      ("t" . pdf-annot-add-text-annotation)
	      ("d" . pdf-annot-delete)
	      ("M-SPC" . pdf-view-scroll-down-or-previous-page)))

(use-package circe
  :bind (:map circe-mode-map
              ("M-n" . "\C-u4\C-v")
	      ( "M-p" . "\C-u4\M-v")))

(use-package q4
  :bind(:map q4-mode-map
	     ("f" . q4/point-to-next-post)
	     ("j" . q4/point-to-next-post)
	     ("e" . q4/open-post-image))
  :config (q4/toggle-thumbnailing-method)
  :defer 5)

;;--------------------------------------------------------------------------------
;; global binds
;;--------------------------------------------------------------------------------
(global-set-key (kbd "H-d") 'hs-toggle-hiding)
;; had this previously set to delete the file, a profoundly stupid idea
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

(maybe-require-package 'bm)
(autoload 'bm-toggle   "bm" "Toggle bookmark in current buffer." t)
(autoload 'bm-next     "bm" "Goto bookmark."                     t)
(autoload 'bm-previous "bm" "Goto previous bookmark."            t)

(global-set-key (kbd "C-c C-b C-t") 'bm-toggle)
(global-set-key (kbd "C-c C-b C-n") 'bm-next)
(global-set-key (kbd "C-c C-b C-p") 'bm-previous)

(maybe-require-package 'caps-lock)
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
(global-set-key (kbd "M-J") 'move-temp-del)
(require 'youdao-dictionary)
(define-key youdao-dictionary-mode-map (kbd "i") 'youdao-extract)
(global-set-key (kbd "H-c") 'collapse-head-whitespace)
(define-key org-mode-map (kbd "H-k") 'avy-jump-open)
;; (global-set-key (kbd "H-y" ) 'avy-yank-line)

(add-hook 'q4-mode-hook
	  (lambda ()
	    (define-key q4-mode-map (kbd "s") 'q4-override/save-post-image)))
(add-hook 'elfeed-show-mode-hook
	  (lambda ()
	    (define-key elfeed-show-mode-map (kbd "o") 'ravar/elfeed-play-enclosure-mpd)))

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

(require 'default-custom)
(when (file-exists-p custom-file)
  (load custom-file))

(provide 'init)
