(defun git-ensure-package (url packdir)
  "This checks that the package exists in .emacs.d/git or else gets it via git clone"
  (let ((dir (expand-file-name (concat "git/" packdir) user-emacs-directory)))
    (unless (file-directory-p dir)
      (make-directory dir t)
      (shell-command (format "git clone '%s' '%s'" url dir)))
    (add-to-list 'load-path dir)))

(provide 'init-git)
