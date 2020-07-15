(defun org-create-formula-image
    (string tofile options buffer &optional processing-type)
  "Create an image from LaTeX source using external processes.

The LaTeX STRING is saved to a temporary LaTeX file, then
converted to an image file by process PROCESSING-TYPE defined in
`org-preview-latex-process-alist'.  A nil value defaults to
`org-preview-latex-default-process'.

The generated image file is eventually moved to TOFILE.

The OPTIONS argument controls the size, foreground color and
background color of the generated image.

When BUFFER non-nil, this function is used for LaTeX previewing.
Otherwise, it is used to deal with LaTeX snippets showed in
a HTML file."
  (let* ((processing-type (or processing-type
			      org-preview-latex-default-process))
	 (processing-info
	  (cdr (assq processing-type org-preview-latex-process-alist)))
	 (programs (plist-get processing-info :programs))
	 (error-message (or (plist-get processing-info :message) ""))
	 (image-input-type (plist-get processing-info :image-input-type))
	 (image-output-type (plist-get processing-info :image-output-type))
	 (post-clean (or (plist-get processing-info :post-clean)
			 '(".dvi" ".xdv" ".pdf" ".tex" ".aux" ".log"
			   ".svg" ".png" ".jpg" ".jpeg" ".out")))
	 (latex-header
	  (or (plist-get processing-info :latex-header)
	      (org-latex-make-preamble
	       (org-export-get-environment (org-export-get-backend 'latex))
	       org-format-latex-header
	       'snippet)))
	 (latex-compiler (plist-get processing-info :latex-compiler))
	 (image-converter (plist-get processing-info :image-converter))
	 (tmpdir temporary-file-directory)
	 (texfilebase (make-temp-name
		       (expand-file-name "orgtex" tmpdir)))
	 (texfile (concat texfilebase ".tex"))
	 (image-size-adjust (or (plist-get processing-info :image-size-adjust)
				'(1.0 . 1.0)))
	 (scale (* (if buffer (car image-size-adjust) (cdr image-size-adjust))
		   (or (plist-get options (if buffer :scale :html-scale)) 1.0)))
	 (dpi (* scale (if buffer (org--get-display-dpi) 140.0)))
	 (fg (or (plist-get options (if buffer :foreground :html-foreground))
		 "Black"))
	 (bg (or (plist-get options (if buffer :background :html-background))
		 "Transparent"))
	 (log-buf (get-buffer-create "*Org Preview LaTeX Output*"))
	 (resize-mini-windows nil)) ;Fix Emacs flicker when creating image.
    (dolist (program programs)
      (org-check-external-command program error-message))
    (if (eq fg 'default)
	(setq fg (org-latex-color :foreground))
      (setq fg (org-latex-color-format fg)))
    (if (eq bg 'default)
	(setq bg (org-latex-color :background))
      (setq bg (org-latex-color-format
		(if (string= bg "Transparent") "white" bg))))
    ;; Remove TeX \par at end of snippet to avoid trailing space.
    (if (string-suffix-p string "\n")
        (aset string (1- (length string)) ?%)
      (setq string (concat string "%")))
    (with-temp-file texfile
      (insert latex-header)
      (insert "\n\\begin{document}\n"
	      "\\definecolor{fg}{rgb}{" fg "}%\n"
	      "\\definecolor{bg}{rgb}{" bg "}%\n"
	      "\n\\pagecolor{bg}%\n"
	      "\n{\\color{fg}\n"
	      string
	      "\n}\n"
	      "\n\\end{document}\n"))
    (let* ((err-msg (format "Please adjust `%s' part of \
`org-preview-latex-process-alist'."
			    processing-type))
	   (image-input-file
	    (org-compile-file
	     texfile latex-compiler image-input-type err-msg log-buf))
	   (image-output-file
	    (org-compile-file
	     image-input-file image-converter image-output-type err-msg log-buf
	     `((?D . ,(shell-quote-argument (format "%s" dpi)))
	       (?S . ,(shell-quote-argument (format "%s" (/ dpi 140.0))))))))
      (kill-buffer log-buf)
      (copy-file image-output-file tofile 'replace)
      (dolist (e post-clean)
	(when (file-exists-p (concat texfilebase e))
	  (delete-file (concat texfilebase e))))
      image-output-file)))
;;now actually shuts up



(defun q4/show-replies (&optional post nomark)
  "Pop a new window to navigate through replies with. The original thread
buffer is left unmodified."
  (interactive)
  (when (and (eq q4/content-type 'replies)
             (consp (car q4/reply-ring))
             (not nomark))
    (setcdr (car q4/reply-ring) (point)))
  (let* ((post (or post (q4/current-post)))
         (replies (q4/get-post-property 'replies post))
         (reply-buffer (get-buffer-create "*Q4 Replies*"))
         (q4/establish-data nil)
         ;; the reply buffer needs to inherit the parent's data,
         ;; so create a function with the parent data and call it
         ;; in the child body
         (set-locals
          `(lambda ()
             (setq-local parent ,(current-buffer))
             (setq-local id ,post)
             (setq q4/metadata     ',q4/metadata
                   q4/content-type 'replies
                   q4/extlink      ,q4/extlink
                   q4/board        ,q4/board
                   q4/threadno     ,q4/threadno))))
    (if (not replies)
        (message "No replies to this post.")
      (with-current-buffer reply-buffer
        (unless (eq major-mode 'q4-mode) (q4-mode))
        (unless nomark (push (cons post nil) q4/reply-ring))
        (setq header-line-format
              (let* ((list
                      (nreverse (mapcar (lambda (x)
                        (concat (propertize (car x) 'face 'q4/quote-face)
                                (propertize " > " 'face 'q4/gray-face)))
                       q4/reply-ring)))
                     (long-p (< (length list) 6)))
                (substring (string-join (append
                  (list (propertize
                         (if long-p " > " "...")
                        'face 'q4/gray-face))
                  (last list 5))) 0 -3)))
        (funcall set-locals)
        (erase-buffer)
        (q4/insert-seperator t)
        (cl-loop for reply in replies
          for alist = (q4/get-post-property 'apidata reply) do
          (q4/with-4chan-binds
           (q4/render-content)
           (q4/insert-seperator)))
        (q4/point-to-first-post)
        (q4/postprocess)
        (when (and q4/thumbnails (display-graphic-p))
          (apply q4/thumbnail-method
                 `(,reply-buffer
                   ,@(when (eq q4/thumbnail-method
                               #'q4/async-thumbnail-dispatch)
                       (list q4/thumblist))))))
      (pop-to-buffer-same-window reply-buffer))))

(defun avy-window-list ()
  "Return a list of windows depending on `avy-all-windows'."
  (cond ((eq avy-all-windows 'all-frames)
         (cl-mapcan #'window-list (visible-frame-list)))

        ((eq avy-all-windows t)
         (window-list))

        ((null avy-all-windows)
         (list (selected-window)))

        (t
         (error "Unrecognized option: %S" avy-all-windows))))

(provide 'patches)
