;;--------------------------------------------------------------------------------
;; setting this so I can keep nonstandard configs for myself
;;--------------------------------------------------------------------------------

(defvar is-ravar)
(setq is-ravar nil)

;;--------------------------------------------------------------------------------
;; handy function for quick dictionary lookup. However since I doubt you will be learning
;; danish anytime soon this function is here mostly to give you an idea how you use it
;; I have custom logic because I am using a local dictionary instead of the dict.org
;; dictionary
;;--------------------------------------------------------------------------------

;;example usage
;;(setq dictionary-server "dict.org")
;;(dictionary-search "sten" "fd-dan-eng")

(if is-ravar (setq dictionary-server "localhost"))

(defun danish-dictionary-at-point ()
	"uses the dictionary package from melpa to search word at point in the fd-dan-eng dictionary"
	(interactive)
	(if is-ravar
	    (dictionary-search (word-at-point) "dan-eng" )
	  (dictionary-searc (word-at-point "fd-dan-eng"))))

;;--------------------------------------------------------------------------------
;; this allow for adjusting the size of latex fragments
;; in tandem with the font size. 
;;--------------------------------------------------------------------------------
(defun my-buffer-scale ()
  "returns the text scale float of the current buffer"
  (interactive)
  (expt text-scale-mode-step text-scale-mode-amount))

(defun org-format-latex--advice (fun &rest r)
  (let (
	(oscale (plist-get org-format-latex-options :scale))
	(mult (my-buffer-scale)))
    (progn
      (plist-put org-format-latex-options :scale
		 (* mult oscale))
      (apply fun r)
      (plist-put org-format-latex-options :scale oscale))))

(add-function :around (symbol-function 'org-format-latex) #'org-format-latex--advice)

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
;; prevent auctex from removing focus from active tex buffer after save
;;--------------------------------------------------------------------------------

(defun frame-return--advice (fun &res r)
  (let (frame (selected-frame))
    (progn
      (apply fun r)
      (select-frame-set-input-focus frame))))

(add-function :around (symbol-function 'TeX-region-update-point)
	      #'frame-return--advice)
(add-function :around (symbol-function 'TeX-region-update)
	      #'frame-return--advice)

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
;; aesthetic centering for full screen reading
;;--------------------------------------------------------------------------------
(defun nov-center-text-after-render ()
  (interactive)
  "add to nov-post-html-render-hook to make the text centered in the frame"
  (cond
   (nov-text-width
    (let* ((width (window-width))
           (offset (- (truncate (* (- width nov-text-width) .5)) 6))
           (prepend (make-string offset ? )))
      (string-insert-rectangle (point-min) (point-max) prepend)))))

(defun nov-save ()
  (interactive)
  "saves the current position"
  (let ((identifier (cdr (assq 'identifier nov-metadata)))
          (index (if (integerp nov-documents-index)
                     nov-documents-index
                   0)))
    (nov-save-place identifier index (point))))

;; this allows nov to correctly render rubies, aka japanese pronunciation hints
(defun shr-tag-rt (dom)
  (let ((start (point)))
    (shr-generic dom)
    ;; (put-text-property start (point) 'display '(height .8))
    (put-text-property start (point) 'display '(raise 0.4))
    (add-face-text-property start (point) '(:height .8))))

;; (defun my-nov-seg-japanese ()
;;   (interactive)
;;   (let* ((html (buffer-substring-no-properties
;; 		(point-min) (point-max)))
;; 	 (new-html
;; 	  (with-temp-buffer
;; 	    (call-process (append "/home/ryan/Desktop/code/jade/jcli" "$'" html "'") nil t )
;; 	    (buffer-substring-no-properties
;; 	     (point-min) (point-max)))))
;;     (goto-char (point-min))
;;     (insert new-html)
;;     (delete-region (point) (point-max))))

;;(add-hook 'nov-pre-html-render-hook 'my-nov-seg-japanese)
(defun my-nov-seg-japanese ()
  (interactive)
  (call-process-region (point-min) (point-max)
		       "/home/ryan/Desktop/code/jade/jcli"
		       t
		       t))

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

(defun clopen-buffer ()
  "derp"
  (interactive)
  (let ((val buffer-file-name))
    (progn (kill-buffer nil)
	   (switch-to-buffer (find-file-noselect val)))))



(provide 'ravar-custom)


