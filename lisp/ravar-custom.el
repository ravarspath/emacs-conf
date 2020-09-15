;;--------------------------------------------------------------------------------
;; This allows for easy cross linking of .org files used to keep research notes
;;--------------------------------------------------------------------------------
(defun rorg-double-link ()
  "yeah"
  (interactive)
  
  (let ((ref-fn nil)
	(orig-buffer (current-buffer))
	(orig-fn (buffer-file-name (current-buffer)))
	(orig-ln (line-number-at-pos)))
    (save-excursion
      (helm-find-files-1 "~/Desktop/research-orgs/concepts/")
      (setq ref-fn (buffer-file-name (current-buffer)))
      (goto-char (point-max))
      (if (eq 0 (funcall outline-level))
	  (progn
	    (org-insert-heading)
	    (insert "Referenced By\n"))
	  (if (not (string= (nth 4 (org-heading-components)) "Referenced By"))
	      (progn
		(org-insert-heading)
		(insert "Referenced By\n"))))
      (insert (concat "[[file:" orig-fn "::" (number-to-string orig-ln)
		      "][" (rorg-abbrev orig-fn) "]]\n" ))
      (save-buffer)
      (kill-buffer))
    (switch-to-buffer orig-buffer)
    (insert (concat "[[file:" ref-fn "][" (rorg-abbrev ref-fn) "]]" ))))

(defun rorg-abbrev (fn)
  "trims file name"
  (let ((abbrev (substring fn (string-match "[a-zA-Z-_]+.[a-zA-Z]+$" fn) nil)))
    (substring abbrev 0 (string-match ".[a-zA-Z]+$" abbrev))))

;;--------------------------------------------------------------------------------
;; silences flyspell for words that are all caps
;;--------------------------------------------------------------------------------
(defun flyspell-ignore-abbrev () (interactive)
       (save-excursion
	 (forward-whitespace -1)
	 (when (looking-at "[ \n]")
	   (forward-char))
	 (not (let ((case-fold-search nil))
		(looking-at "[A-Z]+")))))

(put 'org-mode 'flyspell-mode-predicate 'flyspell-ignore-abbrev)
(put 'text-mode 'flyspell-mode-predicate 'flyspell-ignore-abbrev)

;;--------------------------------------------------------------------------------
;; not really used, this is an attempt to make navigating latex nicer
;;--------------------------------------------------------------------------------
(defun avy-jump-open ()
  (interactive)
  (avy--generic-jump "{" nil)
  (forward-char))

;;--------------------------------------------------------------------------------
;; look at http://www.mycpu.org/emacs-productivity-setup/ ??
;; pulled from https://news.ycombinator.com/item?id=22129636
;;--------------------------------------------------------------------------------

(defun arrayify (start end quote)
    "Turn strings on newlines into a QUOTEd, comma-separated one-liner."
    (interactive "r\nMQuote: ")
    (let ((insertion
           (mapconcatppp
            (lambda (x) (format "%s%s%s" quote x quote))
            (split-string (buffer-substring start end)) ", ")))
      (delete-region start end)
      (insert insertion)))

;;--------------------------------------------------------------------------------
;; allows yanking lines across frames while not chaning location of point
;;--------------------------------------------------------------------------------
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

;;--------------------------------------------------------------------------------
;; macro for cleaning up lisp spacing 
;;--------------------------------------------------------------------------------
(fset 'collapse-head-whitespace
      [?\C-  ?\C-a backspace backspace])

;;turns 
;;( progn
;;     (some expression))
;;into
;;( progn (some expression))

;;--------------------------------------------------------------------------------
;; macro to copy the for a word pinyin out of youdao 
;;--------------------------------------------------------------------------------
(fset 'youdao-extract
      (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([5 2 67108896 5 134217744 6 134217847] 0 "%d")) arg)))

;;--------------------------------------------------------------------------------
;; trying to emulate luke smith functionality
;;--------------------------------------------------------------------------------
(defun move-temp-del ()
  (interactive)
  (progn (search-forward "++")
	 (delete-char -2)))

;;--------------------------------------------------------------------------------
;; used in a yasnippet snippet to speed up typing of chemical formulas
;;--------------------------------------------------------------------------------
(defun chem-convert (text)
  (replace-regexp-in-string "_?[0-9]+$"
      (lambda (x) (format "_%s" (replace-regexp-in-string "_" "" x)))
      text))

;;--------------------------------------------------------------------------------
;; docstring says it all
;;--------------------------------------------------------------------------------
(setq ravar/elfeed-podcast-dir "~/Desktop/music/podcast/")
;;TODO check mimetype first?
(defun ravar/elfeed-play-enclosure-mpd ()
  "Downloads the item in the enclosure and starts in playing in mpd using mpc"
  (interactive)
  (let* ((entry elfeed-show-entry)
	 (enclosure-index (elfeed--get-enclosure-num
			   "Enclosure to save" entry))
         (url-enclosure (car (elt (elfeed-entry-enclosures entry)
                                  (- enclosure-index 1))))
	 (fname
          (funcall elfeed-show-enclosure-filename-function
                   entry url-enclosure)))
    (start-process-shell-command
     "play enclosure" nil
     (format "cd %s; wget %s;mpc update; mpc search filename %s | mpc insert; 
mpc next; mpc play "
	     ravar/elfeed-podcast-dir url-enclosure fname fname))))

;;--------------------------------------------------------------------------------
;; used to allow quick saving of images from q4
;;--------------------------------------------------------------------------------
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

;;--------------------------------------------------------------------------------
;; surprisingly useful missing functionality
;;--------------------------------------------------------------------------------
(defun revert-this-buffer ()
  (interactive)
  (revert-buffer nil t t)
  (message (concat "Reverted buffer " (buffer-name))))

;;--------------------------------------------------------------------------------
;; titles usually self explanatory, not sure what i used them for
;;--------------------------------------------------------------------------------
(fset 'copy-next-line
      (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([14 1 67108896 5 134217847] 0 "%d")) arg)))

(defun my-reverse-region (beg end)
 "Reverse characters between BEG and END."
 (interactive "r")
 (let ((region (buffer-substring beg end)))
   (delete-region beg end)
   (insert (nreverse region))))

(defun add-to-kill (n-str blank) (progn
				   (kill-new n-str )
				   (message "copied: %S" n-str)))

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

(provide 'ravar-custom)
