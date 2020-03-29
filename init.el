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

(electric-pair-mode 1)
(which-key-mode 1)

(require 'key-chord)
(setq key-chord-two-keys-delay 0.2)
(require 'dap-gdb-lldb)
(dap-gdb-lldb-setup)

(require 'rust-mode)
(require 'eldoc)
(require 'racer)
(require 'flycheck-rust)
(require 'cargo)
(require 'company)
(require 'flycheck)
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

(define-key rust-mode-map (kbd "-") (lambda () (interactive) (insert-char #x5f)))
(define-key rust-mode-map (kbd "_") (lambda () (interactive) (insert-char #x2d)))
(define-key rust-mode-map (kbd "C-c C-a") 'company-mode)
(define-key rust-mode-map (kbd "H-r") 'lsp-rename)
(define-key rust-mode-map (kbd "H-l") 'lsp-avy-lens)
(define-key rust-mode-map (kbd "H-m H-l") 'lsp-lens-mode)

;; is lense mode any good for rust??
(rust-enable-format-on-save)
(setq rust-match-angle-brackets nil)
(setq exec-path(append exec-path '("~/.cargo/bin")))
(setq racer-cmd "~/.cargo/bin/racer")
(setq racer-rust-src-path "~/code/rust/src/")

;; (use-package lsp-mode
;;   :commands lsp
;;   :config (require 'lsp-clients))
;; (use-package lsp-ui)
;; don't like lsp, its too clever by half
;; warning about ptrace scope you unhardend this
;; (keyboard-translate ?\- ?\-)
;; (keyboard-translate ?\_ ?\_) saving because intersting syntax

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



(load-theme 'monokai t)
;; (load-theme 'deeper-blue t)
;;super bright, but does handle org
;; (load-theme 'leuven t)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((ipython . t)
   (clojure . t)
   (J . t)
   ;; other languages..
   ))

(add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)
(setq org-confirm-babel-evaluate nil)
(define-key org-mode-map (kbd "H-[") 'org-agenda)
(key-chord-define org-mode-map "qi" '(lambda () (interactive) (insert "\\")))

(desktop-save-mode 1)

(global-linum-mode)
(defun buffer-too-big-p ()
  (or (> (buffer-size) (* 5000 80))
      (> (line-number-at-pos (point-max)) 5000)))
(add-hook 'prog-mode-hook
          (lambda ()
            ;; turn off `linum-mode' when there are more than 5000 lines
            (if (buffer-too-big-p) (linum-mode -1))))

(pdf-loader-install)
(add-hook 'pdf-view-mode-hook (lambda () (linum-mode 0)))

(linum-mode t)

(auto-revert-mode)

(global-set-key (kbd "H-d") 'delete-this-file)
(global-set-key (kbd "H-r") 'rename-this-file-and-buffer)

(global-set-key (kbd "C-f") 'forward-char)
(global-set-key (kbd "M-f") 'forward-word)

(global-set-key (kbd "C-b") 'backward-char)
(global-set-key (kbd "M-b") 'backward-word)


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

(global-set-key "\M-n" "\C-u4\C-v")
(global-set-key "\M-p" "\C-u4\M-v")
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

;; (require 'palette)

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

;; (use-package sunrise
;;   :load-path "~/.emacs.d/addedPackages/sunrise-commander"
;;   )
;; (add-to-list 'auto-mode-alist '("\\.srvm\\'" . sunrise-virtual-mode))
;; (global-set-key (kbd "C-x C-d") 'sunrise-cd)
;; (define-key sunrise-mode-map (kbd "C-;") 'dired-subtree-remove)


(require 'dired-x)
(add-hook 'dired-load-hook
	  (function (lambda () (load "dired-x"))))

(add-hook 'dired-mode-hook (lambda () (dired-omit-mode)))

(require 'flyspell)
(add-hook 'markdown-mode-hook 'flyspell-mode)
(define-key flyspell-mode-map (kbd "C-c C-d") 'ispell-word)

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(require 'i3-integration)
(i3-one-window-per-frame-mode-on)
;; (i3-advice-visible-frame-list-on)

(require 'pdf-tools)
(define-key pdf-view-mode-map (kbd "H") 'pdf-annot-add-highlight-markup-annotation)
(define-key pdf-view-mode-map (kbd "t") 'pdf-annot-add-text-annotation)
(define-key pdf-view-mode-map (kbd "d") 'pdf-annot-delete)
(define-key pdf-view-mode-map (kbd "M-SPC") 'pdf-view-scroll-down-or-previous-page)

(require 'keyfreq)
(keyfreq-mode 1)
(keyfreq-autosave-mode 1)

(global-set-key (kbd "C-c m c") 'mc/edit-lines)
(global-set-key (kbd "<M-right>") 'mc/mark-next-like-this)
(global-set-key (kbd "<M-left>") 'mc/mark-previous-like-this)

(global-set-key [f10] 'magit-status)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key [f1] 'ansi-term)

(defun revert-this-buffer ()
  (interactive)
  (revert-buffer nil t t)
  (message (concat "Reverted buffer " (buffer-name))))
(global-set-key [f6] 'revert-this-buffer)

(eval-when-compile
  (require 'cl))

(defun get-buffers-matching-mode (mode)
  "Returns a list of buffers where their major-mode is equal to MODE"
  (let ((buffer-mode-matches '()))
   (dolist (buf (buffer-list))
     (with-current-buffer buf
       (if (eq mode major-mode)
           (add-to-list 'buffer-mode-matches buf))))
   buffer-mode-matches))

(defun multi-occur-in-this-mode ()
  "Show all lines matching REGEXP in buffers with this major mode."
  (interactive)
  (multi-occur
   (get-buffers-matching-mode major-mode)
   (car (occur-read-primary-args))))

;; global key for `multi-occur-in-this-mode' - you should change this.
(global-set-key (kbd "C-<f2>") 'multi-occur-in-this-mode)

(require 'helm-config)
(helm-mode 1)
(global-set-key "\C-x\C-f" 'helm-find-files)
(global-set-key (kbd "C-x k") (lambda () (interactive) (kill-buffer nil)))
(global-set-key (kbd "M-x") 'helm-M-x)
(define-key helm-map (kbd "C-z")  'helm-select-action)
(define-key helm-find-files-map (kbd "C-<backspace>") 'backward-kill-word)
(define-key helm-read-file-map (kbd "C-<backspace>") 'backward-kill-word)
(define-key helm-find-files-map (kbd "<tab>") 'ignore ) 
(define-key helm-read-file-map (kbd "<tab>") 'ignore)
(global-set-key (kbd "C-c C-s") 'helm-occur)
(require 'helm-descbinds)
(helm-descbinds-mode)
(require 'helm-dictionary)
(define-key helm-command-map (kbd "d") 'helm-dictionary)
(define-key helm-command-map (kbd "C-s") 'helm-occur)
(global-set-key (kbd "H-i") 'helm-imenu)
(define-key helm-command-map (kbd "C-g") 'helm-grep-do-git-grep)
;;TODO integrate helm with helpful??


(fset 'yes-or-no-p 'y-or-n-p)

(require 'use-package)

(show-paren-mode 1)

(require 'markdown-mode)
(add-hook 'markdown-mode-hook 'visual-line-mode)
(define-key markdown-mode-map "\M-n" "\C-u4\C-v")
(define-key markdown-mode-map "\M-p" "\C-u4\M-v")

(require 'god-mode)
(global-set-key (kbd "<escape>") 'god-local-mode)
(define-key god-local-mode-map "i" 'god-local-mode)
(define-key god-local-mode-map "z" 'repeat)

(use-package avy
  :load-path "~/Desktop/code/custemacs/avy"
  )

(global-set-key (kbd "C-;") 'avy-goto-char)
(global-set-key (kbd "C-j") 'avy-goto-word-or-subword-1)
(global-set-key (kbd "M-j") 'avy-goto-line)
(define-key org-mode-map (kbd "C-j") 'avy-goto-word-or-subword-1)
(define-key LaTeX-mode-map (kbd "C-j") 'avy-goto-word-or-subword-1)
(define-key flyspell-mode-map (kbd "C-;") 'avy-goto-char)
(define-key lisp-interaction-mode-map (kbd "C-j") 'avy-goto-word-or-subword-1)
(define-key lisp-interaction-mode-map (kbd "C-M-j") 'eval-print-last-sexp)
(global-set-key (kbd "C-M-j") "\C-\M-u\C-\M-f")
(global-set-key (kbd "C-M-n") 'forward-list)


(define-key org-mode-map (kbd "H-o") 'helm-occur)
(define-key org-mode-map (kbd "H-l") 'rorg-double-link)
;; (pyvenv-activate "~/envs/benv")


;; (add-hook 'after-init-hook 'global-flycheck-mode)
;; flycheck is nice, but really agressive, I don't want it for hacks
;;hacks like .emacs files

(set-face-attribute 'default nil :height 85)


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

(autoload 'bm-toggle   "bm" "Toggle bookmark in current buffer." t)
(autoload 'bm-next     "bm" "Goto bookmark."                     t)
(autoload 'bm-previous "bm" "Goto previous bookmark."            t)

(global-set-key (kbd "C-c C-b C-t") 'bm-toggle)
(global-set-key (kbd "C-c C-b C-n") 'bm-next)
(global-set-key (kbd "C-c C-b C-p") 'bm-previous)

(require 'caps-lock)
(global-set-key (kbd "M-c") 'caps-lock-mode)

(require 'disable-mouse)
(global-disable-mouse-mode)

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

(global-set-key (kbd "M-s") 'paredit-splice-sexp)

(require 'elfeed)
(define-key elfeed-show-mode-map (kbd "<return>") 'shr-copy-url)
(define-key elfeed-search-mode-map (kbd "C-l") 'elfeed-update)
(define-key elfeed-search-mode-map (kbd "k") 'next-line)
(define-key elfeed-search-mode-map (kbd "l") 'previous-line)
;; press S too change filter

(require 'tabbar)
(global-set-key (kbd "C-s-;") 'tabbar-forward-tab)
(global-set-key (kbd "C-s-j") 'tabbar-backward-tab)
(add-hook 'tabbar-mode-hook (lambda () (interactive) (remove-hook 'kill-buffer-hook 'tabbar-buffer-track-killed)))
(remove-hook 'kill-buffer-hook 'tabbar-buffer-track-killed)

(require 'youdao-dictionary)

(add-to-list 'load-path "~/.emacs.d/addedPackages/q4")
(require 'q4)
(global-set-key (kbd "H-0") 'q4/browse-board)
(q4/toggle-thumbnailing-method)

(defun q4-override/wget-image (addr)
  "This will be attached to the mouse action for a button 
and will be used to save the associated image"
  (interactive)
  (start-process-shell-command
   "q4-save-im" nil (format "cd ~/Pictures/4chan; wget %s " addr)))

(defun q4-override/save-post-image ()
  "save the posts current image in specified directory"
  (interactive)
  (save-excursion
    (q4/assert-post-start)
    (let ((image (q4/next-prop 'image nil (q4/sep-pos))))
      (if image (push-button image t)
        (message "No image in this post.")))))

(define-key q4-mode-map (kbd "s") 'q4-override/save-post-image)

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


(defun add-to-kill (n-str blank) (progn
				   (kill-new n-str )
				   (message "copied: %S" n-str)))

(defun chem-convert (text)
  (replace-regexp-in-string "_?[0-9]+$"
      (lambda (x) (format "_%s" (replace-regexp-in-string "_" "" x)))
text))

(defun my-reverse-region (beg end)
 "Reverse characters between BEG and END."
 (interactive "r")
 (let ((region (buffer-substring beg end)))
   (delete-region beg end)
   (insert (nreverse region))))

(defun move-temp-del ()
  (interactive)
  (progn (search-forward "++")
	 (delete-char -2)))
(global-set-key (kbd "M-J") 'move-temp-del)

(fset 'copy-next-line
      (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([14 1 67108896 5 134217847] 0 "%d")) arg)))

(fset 'youdao-extract
      (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([5 2 67108896 5 134217744 6 134217847] 0 "%d")) arg)))

(define-key youdao-dictionary-mode-map (kbd "i") 'youdao-extract)

(fset 'collapse-head-whitespace
      [?\C-  ?\C-a backspace backspace])

(global-set-key (kbd "H-c") 'collapse-head-whitespace)

(defun avy-yank-line (u)
  (interactive "P")
  (let ((avy-all-windows 'all-frames)
	(start-frame (selected-frame)))
    (save-excursion (avy--generic-jump "\n" nil)
		    (move-beginning-of-line nil)
		    (set-mark-command nil)
		    (move-end-of-line nil)
		    (if u (kill-region 0 1 1) (kill-ring-save 0 1 1)))
    (select-frame-set-input-focus start-frame))) 

(global-set-key (kbd "H-y" ) 'avy-yank-line)

;; look at http://www.mycpu.org/emacs-productivity-setup/ ??
;; pulled from https://news.ycombinator.com/item?id=22129636
(defun arrayify (start end quote)
    "Turn strings on newlines into a QUOTEd, comma-separated one-liner."
    (interactive "r\nMQuote: ")
    (let ((insertion
           (mapconcatppp
            (lambda (x) (format "%s%s%s" quote x quote))
            (split-string (buffer-substring start end)) ", ")))
      (delete-region start end)
      (insert insertion)))

(defun avy-jump-open ()
  (interactive)
  (avy--generic-jump "{" nil)
  (forward-char))
(define-key org-mode-map (kbd "H-k") 'avy-jump-open)

(defun flyspell-ignore-abbrev () (interactive)
       (save-excursion
	 (forward-whitespace -1)
	 (when (looking-at "[ \n]")
	   (forward-char))
	 (not (let ((case-fold-search nil))
		(looking-at "[A-Z]+")))))

(put 'org-mode 'flyspell-mode-predicate 'flyspell-ignore-abbrev)

(defun rorg-abbrev (fn)
  "trims file name"
  (let ((abbrev (substring fn (string-match "[a-zA-Z-_]+.[a-zA-Z]+$" fn) nil)))
    (substring abbrev 0 (string-match ".[a-zA-Z]+$" abbrev))))

(yas-global-mode)

(require 'diminish)
;; (diminish 'lsp-mode "LSP")
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
;; Many default configurations are frustrating, loads some modified functions
;;--------------------------------------------------------------------------------
(require 'patches)

(provide 'init)
