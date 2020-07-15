(custom-set-variables
 '(browse-url-browser-function (quote add-to-kill))
 '(browse-url-firefox-new-window-is-tab t)
 '(calendar-setup (quote calendar-only))
 '(case-fold-search t)
 '(default-input-method "pyim")
 '(diary-number-of-entries 3)
 '(ediff-window-setup-function (quote ediff-setup-windows-multiframe))
 '(elfeed-curl-max-connections 16)
 '(flycheck-keymap-prefix "1")
 '(helm-command-prefix-key "<C-return>")
 '(lsp-prefer-flymake :none)
 '(lsp-rust-build-on-save nil)
 '(max-mini-window-height 0.75)
 '(org-agenda-window-setup (quote other-window))
 '(org-file-apps
   (quote
    ((auto-mode . emacs)
     ("\\.mm\\'" . default)
     ("\\.x?html?\\'" . default)
     ("\\.pdf\\'" . emacs))))
 '(org-todo-keywords (quote ((sequence "TODO" "DONE"))))
 '(tabbar-background-color "gray11")
 '(tabbar-home-button (quote (("") "")))
 '(tabbar-mode t nil (tabbar))
 '(tabbar-mwheel-mode t nil (tabbar))
 '(tabbar-scroll-left-button (quote ((" ") "")))
 '(tabbar-scroll-right-button (quote (("") "")))
 '(tabbar-separator (quote (1.0)))
 '(tabbar-use-images nil)
 '(web-mode-extra-snippets (quote (("url" . "{% url | %}") ("code" . "{% | %}"))))
 '(writegood-weasel-words
   (quote
    ("many" "various" "very" "fairly" "several" "extremely" "exceedingly" "quite" "remarkably" "few" "surprisingly" "mostly" "largely" "huge" "tiny" "are a number" "is a number" "excellent" "interestingly" "significantly" "substantially" "clearly" "vast" "relatively" "completely" "literally" "not rocket science" "outside the box" "helps" "supports" "is useful" "better" "improved" "gains" "acts" "works" "effective" "efficient" "seems" "appears" "looks" "is like" "most" "virtually" "almost all" "up to" "from" "at least" "as many as"))))

(custom-set-faces
 '(tabbar-button ((t (:inherit tabbar-default :box (:line-width 1 :color "white")))))
 '(tabbar-default ((t (:inherit variable-pitch :background "gray11" :foreground "grey90" :height 0.8))))
 '(tabbar-modified ((t (:inherit tabbar-default :foreground "green" :box (:line-width 1 :color "white")))))
 '(tabbar-selected ((t (:inherit tabbar-default :foreground "dodger blue" :box (:line-width 1 :color "white")))))
 '(tabbar-selected-modified ((t (:inherit tabbar-default :foreground "dodger blue" :box (:line-width 1 :color "white"))))))

(provide 'default-custom)
