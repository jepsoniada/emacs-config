(defmacro bind-shell-command (name)
  "creates binding for shell command named NAME
also defines feature NAME"

  `(when (executable-find ,(symbol-name name))
     (defun ,name (&rest args)
       (with-temp-buffer
	 (eval (append '(call-process ,(symbol-name name) nil (current-buffer) nil) args))
	 (goto-char 0)
	 (read (current-buffer))))
     
     (provide ',name)))

(provide 'shell-command)
