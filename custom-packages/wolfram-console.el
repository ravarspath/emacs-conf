
(require 'comint)

(defgroup wolfram-console nil
  "REPL integration extention for `wolfram'"
  :group 'applications
  :group 'wolfram
  :prefix "wolfram-console-")


(defcustom wolfram-console-cmd-args '()
  "Arguments to be passed to the wolfram-console-cmd on start"
  :type 'string
  :group 'wolfram-console)

(defcustom wolfram-console-cmd-init-file nil
  "Full path to the file who's contents are sent to the
  wolfram-console-cmd on start

Should be NIL if there is no file not the empty string"
  :type 'string
  :group 'wolfram-console)

(defcustom wolfram-console-cmd "wolfram"
  "Name of the executable used for the wolfram REPL session"
  :type 'string
  :group 'wolfram-console)

(defcustom wolfram-console-cmd-buffer-name "wolfram"
  "Name of the buffer which contains the wolfram-console-cmd session"
  :type 'string
  :group 'wolfram-console)

(defun wolfram-console-create-session ()
  "Starts a comint session wrapped around the wolfram-console-cmd"
  (setq comint-process-echoes t)
  (apply 'make-comint wolfram-console-cmd-buffer-name
         wolfram-console-cmd wolfram-console-cmd-init-file wolfram-console-cmd-args))
  ;;TODO add hooks?
  ;; (mapc
  ;;  (lambda ( comint-hook-sym )
  ;;    (let ((local-comint-hook-fn-sym
  ;;           (intern
  ;;            (replace-regexp-in-string
  ;;             "s$" "" (concat "j-console-" (symbol-name comint-hook-sym))))))
  ;;      (when (symbol-value local-comint-hook-fn-sym)
  ;;        (add-hook comint-hook-sym (symbol-value local-comint-hook-fn-sym)))))
  ;;  '(comint-input-filter-functions
  ;;    comint-output-filter-functions
  ;;    comint-preoutput-filter-functions))


(defun wolfram-console-ensure-session ()
  "Checks for a running wolfram-console-cmd comint session and either
  returns it or starts a new session and returns that"
  (or (get-process wolfram-console-cmd-buffer-name)
      (progn
        (wolfram-console-create-session)
        (get-process wolfram-console-cmd-buffer-name))))


(provide 'wolfram-console)
