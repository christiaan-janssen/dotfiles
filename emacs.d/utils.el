;; utils.el

(defun check-or-create-dir (dir)
  (unless (file-directory-p dir)
    (make-directory dir)))

(defun load-config-from-file (filename)
  (when (file-exists-p filename)
      (load-file filename)))

(defun async-shell-command-no-window
    (command)
  (interactive)
  (let
      ((display-buffer-alist
        (list
         (cons
          "\\*Async Shell Command\\*.*"
          (cons #'display-buffer-no-window nil)))))
    (async-shell-command
     command)))

(defun run-dragon-ruby ()
  (interactive)
  (async-shell-command-no-window "./run"))
