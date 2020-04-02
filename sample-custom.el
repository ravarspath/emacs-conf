(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(browse-url-browser-function (quote add-to-kill))
 '(browse-url-firefox-new-window-is-tab t)
 '(calendar-setup (quote calendar-only))
 '(case-fold-search t)
 '(default-input-method "pyim")
 '(diary-number-of-entries 3)
 '(disable-mouse-global-mode nil nil (disable-mouse))
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
 '(org-structure-template-alist
   (quote
    (("src" "#+BEGIN_SRC ?

#+END_SRC" "")
     ("n" "#+NAME: ?" "")
     ("d" "#+BEGIN_DEFINE ?

#+END_DEFINE" "<define>
?
</define>")
     ("ipyim" "#+BEGIN_SRC ipython :session :async :results raw drawer
?
#+END_SRC" "<src land=\"ipython\">
?
</src>")
     ("ipyc" "#+BEGIN_SRC ipython :session /run/user/1001/jupyter/kernel-?.json :async :results output 

#+END_SRC" "")
     ("ipy" "#+BEGIN_SRC ipython :session :async :results output 
?
#+END_SRC" "<src land=\"ipython\">
?
</src>")
     ("s" "#+BEGIN_SRC ?

#+END_SRC" "<src lang=\"?\">

</src>")
     ("e" "#+BEGIN_EXAMPLE
?
#+END_EXAMPLE" "<example>
?
</example>")
     ("v" "#+BEGIN_VERSE
?
#+END_VERSE" "<verse>
?
</verse>")
     ("V" "#+BEGIN_VERBATIM
?
#+END_VERBATIM" "<verbatim>
?
</verbatim>")
     ("c" "#+BEGIN_CENTER
?
#+END_CENTER" "<center>
?
</center>")
     ("l" "#+BEGIN_LaTeX
?
#+END_LaTeX" "<literal style=\"latex\">
?
</literal>")
     ("L" "#+LaTeX: " "<literal style=\"latex\">?</literal>")
     ("h" "#+BEGIN_HTML
?
#+END_HTML" "<literal style=\"html\">
?
</literal>")
     ("H" "#+HTML: " "<literal style=\"html\">?</literal>")
     ("a" "#+BEGIN_ASCII
?
#+END_ASCII" "")
     ("A" "#+ASCII: " "")
     ("i" "#+INDEX: ?" "#+INDEX: ?")
     ("I" "#+INCLUDE: %file ?" "<include file=%file markup=\"?\">"))))
 '(org-todo-keywords (quote ((sequence "TODO" "DONE"))))
 '(package-selected-packages
   (quote
    (multiple-cursors exec-path-from-shell gnu-elpa-keyring-update fullframe seq)))
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
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(tabbar-button ((t (:inherit tabbar-default :box (:line-width 1 :color "white")))))
 '(tabbar-default ((t (:inherit variable-pitch :background "gray11" :foreground "grey90" :height 0.8))))
 '(tabbar-modified ((t (:inherit tabbar-default :foreground "green" :box (:line-width 1 :color "white")))))
 '(tabbar-selected ((t (:inherit tabbar-default :foreground "dodger blue" :box (:line-width 1 :color "white")))))
 '(tabbar-selected-modified ((t (:inherit tabbar-default :foreground "dodger blue" :box (:line-width 1 :color "white"))))))
