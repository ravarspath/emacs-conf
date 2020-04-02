
(setq using-i3 t)
(if using-i3
    (progn
      (git-ensure-package "https://github.com/vava/i3-emacs.git" "i3-integration")
      (require 'i3-integration)
      (i3-one-window-per-frame-mode-on)))

(setq using-langtool t)
(setq langtool-jar-dir "~/software/LanguageTool-4.6/languagetool-commandline.jar")
(if using-langtool
    (progn
      (git-ensure-package "https://github.com/mhayashi1120/Emacs-langtool" "emacs-langtool")
      (setq langtool-language-tool-jar langtool-jar-dir)
      (require 'langtool)
      (with-eval-after-load "markdown-mode"
	(progn
	  (define-key markdown-mode-map (kbd "C-c C-v C-c") 'langtool-check)
	  (define-key markdown-mode-map (kbd "C-c C-v C-d") 'langtool-check-done)))))

(provide 'init-optional)
