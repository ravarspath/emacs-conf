

;; Distributed under the GNU GPL v2 or later



;;TODO make output formatting sane
;;TODO capture output from first snippet sent to console

(require 'ob)

(defcustom org-babel-wolfram-command "wolfram"
  "Command to run wolfram"
  :group 'org-babel
  :version "26.3"
  :package-version '(Org . "9.0")
  :type 'string)

(defun org-babel-expand-body:wolfram (body _params)
  "Expand Body according to PARAMS, return the expanded body. except not, this is a lazy
dumb implementation"
  body)

(defun org-babel-execute:wolfram (body params)
  "Execute a block of wolfram code BODY.
PARAMS are given by org-babel.
This function is called by `org-babel-execute-src-block'"
  (message "executing wolfram source code block")
  (let* ((sessionp (cdr (assq :session params)))
	 (full-body (org-babel-expand-body:wolfram body params)))
    (org-babel-wolfram-initiate-session sessionp)
    (if (string= sessionp "none")
	(
	 ;;TODO no session
	 )
	 (org-babel-wolfram-eval-string full-body))))

(defun org-babel-wolfram-eval-string (str)
  "Sends STR to the `wolfram-console-cmd' session and executes it."
  (let ((session (wolfram-console-ensure-session)))
    (with-current-buffer (process-buffer session)
      (goto-char (point-max))
      (insert (format "%s\n" str))
      (let ((beg (point)))
	(comint-send-input)
	(sit-for .1)
	(buffer-substring-no-properties
	 beg (point-max))))))


(defun org-babel-wolfram-initiate-session (&optional session)
  "Initiate a wolfram session.
SESSION is a parameter given by org-babel."
  (unless (string= session "none")
    (require 'wolfram-console)
    (wolfram-console-ensure-session)))


(provide 'ob-wolfram)
