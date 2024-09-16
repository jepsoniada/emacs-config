(add-to-list 'load-path "~/.config/emacs/lisp")

;;; melpa (bleading-edge)
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

;;; termux compatibility
(setenv "PATH" (format "%s:%s"
		       "/data/data/com.termux/files/usr/bin"
		       (getenv "PATH")))
(push "/data/data/com.termux/files/usr/bin"
      exec-path)

;;; util

(defun func-def (function-symbol)
  "return lisp function definition"
  (car (read-from-string (save-window-excursion
			   (find-function function-symbol)
			   (set-mark (point))
			   (forward-sexp)
			   (buffer-substring-no-properties (mark) (point))))))

;;; keybindings
(defun keyboard-escape-quit ()
  "Exit the current \"mode\" (in a generalized sense of the word).
This command can exit an interactive command such as `query-replace',
can clear out a prefix argument or a region,
can get out of the minibuffer or other recursive edit,
cancel the use of the current buffer (for special-purpose buffers),
or go back to just one window (by deleting all but the selected window)."
  (interactive)
  (cond ((eq last-command 'mode-exited) nil)
	((region-active-p)
	 (deactivate-mark))
	((> (minibuffer-depth) 0)
	 (abort-recursive-edit))
	(current-prefix-arg
	 nil)
	((> (recursion-depth) 0)
	 (exit-recursive-edit))
	(buffer-quit-function
	 (funcall buffer-quit-function))
	;; ((not (one-window-p t))
	;;  (delete-other-windows))
	((string-match "^ \\*" (buffer-name (current-buffer)))
	 (bury-buffer))))
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(define-key input-decode-map (kbd "<volume-down>") (kbd "<escape>"))
(define-key input-decode-map (kbd "<volume-up>") (kbd "<tab>"))
;; (defun +keyboard-escape-quit-adv (fun)
;;   "Around advice for `keyboard-escape-quit' FUN. Preserve window configuration when pressing ESC."
;;   (let ((buffer-quit-function (or buffer-quit-function #'ignore)))
;;     (funcall fun)))
;; (advice-add #'keyboard-escape-quit :around #'+keyboard-escape-quit-adv)

(keymap-global-set "C-M-n" #'down-list)

(defun jepson/up-list (&rest _)
  (interactive)
  (up-list)
  (backward-sexp nil t))

(keymap-global-set "C-M-p" #'jepson/up-list)

(setq frame-resize-pixelwise t
      browse-url-browser-function #'eww-browse-url
      overriding-text-conversion-style nil
      touch-screen-display-keyboard t)
(electric-pair-mode t)

;;; god mode
(when (and (ignore-errors (require 'god-mode))
	   (ignore-errors (require 'god-mode-isearch))
	   (null (getenv "TITAN")))

  ;; (god-mode-all 1)
  (add-to-list 'window-state-change-hook (lambda nil
					   (when (not (minibufferp))
					     (god-local-mode 1))))
  (setq god-exempt-major-modes nil
	god-exempt-predicates nil)
  (setq-default mode-line-format
		'((:eval
		   (if god-local-mode
		       (propertize " god " 'face '((t (:background "#0F0"))))
		     (propertize " not " 'face '((t (:background "#F00"))))))
		  "%e"
		  mode-line-front-space
		  (:propertize
		   ("" mode-line-mule-info mode-line-client mode-line-modified
		    mode-line-remote mode-line-window-dedicated)
		   display
		   (min-width (6.0)))
		  mode-line-frame-identification
		  mode-line-buffer-identification
		  "   "
		  mode-line-position
		  (project-mode-line project-mode-line-format)
		  (vc-mode vc-mode)
		  "  "
		  mode-line-modes
		  mode-line-misc-info
		  mode-line-end-spaces))

  ;; (define-key input-decode-map (kbd "C-i") (kbd "<C-i>"))
  (global-set-key (kbd "C-z") #'god-local-mode)
  (global-set-key (kbd "π") #'god-local-mode)

  (define-key god-local-mode-map (kbd ".") #'repeat)
  (define-key god-local-mode-map (kbd "z") #'god-local-mode)

  (define-key isearch-mode-map (kbd "C-z") 'god-mode-isearch-activate)
  (define-key isearch-mode-map (kbd "π") 'god-mode-isearch-activate)
  (define-key god-mode-isearch-map (kbd "z") 'god-mode-isearch-disable)
  (define-key god-mode-isearch-map (kbd "<escape>") 'god-mode-isearch-disable))

;;; org mode
(when (ignore-errors (require 'org))

  (dolist (face '(
		  (org-level-1 . 2.0)
		  (org-level-2 . 1.75)
		  (org-level-3 . 1.5)
		  (org-level-4 . 1.25)
		  (org-level-5 . 1.0)
		  (org-level-6 . 1.0)
		  (org-level-7 . 1.0)
		  (org-level-8 . 1.0)))
    (set-face-attribute (car face) nil :height (cdr face)))
  (setq org-src-window-setup 'current-window
	org-startup-indented t
	org-default-notes-file (concat org-directory "/notes.org"))
  (define-key org-mode-map (kbd "C-<") #'org-metaleft)
  (define-key org-mode-map (kbd "C->") #'org-metaright))

(ignore-errors (require 'tramp))

(ignore-errors (require 'tetris))

(ignore-errors (require 'typetest))

(when (ignore-errors (require 'avy))

  (keymap-global-set "C-; C-j" 'avy-goto-char)
  (keymap-global-set "C-; C-l" 'avy-goto-line))

(when (ignore-errors (require 'ob-J))

  (defun org-babel-J-aproximate-list-shape (list)
    (when (listp list)
      (nconc (list (length list))
	     (if (listp (car list))
		 (org-babel-J-aproximate-list-shape (car list))
	       nil))))

  (defun org-babel-J-var-to-J-items (data)
    (cond ((numberp data)
	   (number-to-string data)
	   )
	  ((string-empty-p data)
	   "_."
	   )
	  (t (error "data type not implemented")
	     )))

  (defun org-babel-J-var-to-J (data)
    (cond ((listp data)
	   (let ((flatten-list (flatten-list data)))
	     (format "%s $ %s"
		     (mapconcat #'number-to-string
				(org-babel-J-aproximate-list-shape data)
				" ")
		     (if (eval (cons 'and (mapcar (lambda (a) (or (numberp a)
								  (string-empty-p a)))
						  flatten-list)))
			 (mapconcat #'org-babel-J-var-to-J-items
				    flatten-list
				    " ")
		       (mapconcat #'org-babel-J-var-to-J
				  flatten-list
				  " ; "))))
	   )
	  ((stringp data)
	   (format "'%s'"
		   data)
	   )
	  ((numberp data)
	   (number-to-string data)
	   )
	  (t (error "data type not implemented")
	     )))

  (defun org-babel-J-body-enclosure (body)
    "encapsulates source body into a verb definition"
    (format "{{\n%s\n}} 0"
	    body))

  (defun org-babel-expand-body:J (body params &optional _processed-params)
    "Expand BODY according to PARAMS, return the expanded body.
PROCESSED-PARAMS isn't used yet."
    (let ((vars (org-babel--get-vars params)))
      (format "%s\n%s"
	      (apply 'concat (mapcar (lambda (x)
				       (format "%s =: %s\n"
					       (car x)
					       (org-babel-J-var-to-J (cdr x))))
				     vars))
	      (org-babel-J-body-enclosure body))))
  ;; (defun org-babel-expand-body:J (body params &optoinal pparams)
  ;;   "Expand BODY according to PARAMS, return the expanded body."
  ;;   (let ((vars (org-babel--get-vars params)))
  ;;     (org-babel-J-interleave-echos-except-functions body)))

  (defun org-babel-execute:J (body params)
    "Execute a block of J code BODY.
PARAMS are given by org-babel.
This function is called by `org-babel-execute-src-block'."
    (message "executing J source code block")
    (let* ((processed-params (org-babel-process-params params))
	   (sessionp (cdr (assq :session params)))
	   (sit-time (let ((sit (assq :sit params)))
		       (if sit (cdr sit) .1)))
           (full-body (org-babel-expand-body:J
                       body params processed-params))
	   (tmp-script-file (org-babel-temp-file "J-src")))
      ;; (org-babel-j-initiate-session sessionp)

      ;; full-body

      (progn
	(with-temp-file tmp-script-file
	  (insert full-body))
	(org-babel-eval (format "%s < %s" org-babel-J-command tmp-script-file) ""))

      ;; (org-babel-J-eval-string full-body sit-time)

      ;; (org-babel-eval (format "%s < %s" org-babel-J-command
      ;; 			    (with-temp-file tmp-script-file
      ;; 			      (insert full-body)))
      ;; 		    "")

      ;; (org-babel-result-cond result-params
      ;;   (let ((print-level nil)
      ;;         (print-length nil))
      ;;     (if (or (member "scalar" result-params)
      ;;             (member "verbatim" result-params))
      ;;         (format "%S" result)
      ;;       (format "%s" result)))
      ;;   (org-babel-reassemble-table
      ;;    result
      ;;    (org-babel-pick-name (cdr (assq :colname-names params))
      ;;                         (cdr (assq :colnames params)))
      ;;    (org-babel-pick-name (cdr (assq :rowname-names params))
      ;;                         (cdr (assq :rownames params)))))
      
      ;; (org-babel-J-strip-whitespace
      ;;  (if (string= sessionp "none")
      ;; 	 (progn
      ;; 	   (with-temp-file tmp-script-file
      ;; 	     (insert full-body))
      ;; 	   (org-babel-eval (format "%s < %s" org-babel-J-command tmp-script-file) ""))
      ;;    (org-babel-J-eval-string full-body sit-time)))
      ))

  (defun org-babel-execute:emacs-lisp (body params)
    "Execute a block of emacs-lisp code with Babel."
    (let* ((lexical (cdr (assq :lexical params)))
	   (result-params (cdr (assq :result-params params)))
	   (body (format (if (member "output" result-params)
			     "(with-output-to-string %s\n)"
			   "(progn %s\n)")
			 (org-babel-expand-body:emacs-lisp body params)))
	   (result (eval (read (if (or (member "code" result-params)
				       (member "pp" result-params))
				   (concat "(pp " body ")")
				 body))
			 nil
			 ;; (org-babel-emacs-lisp-lexical lexical)
			 )))
      result

      ;; (org-babel-result-cond result-params
      ;;   (let ((print-level nil)
      ;;         (print-length nil))
      ;;     (if (or (member "scalar" result-params)
      ;;             (member "verbatim" result-params))
      ;;         (format "%S" result)
      ;;       (format "%s" result)))
      ;;   ;; (org-babel-reassemble-table
      ;;   ;;  result
      ;;   ;;  (org-babel-pick-name (cdr (assq :colname-names params))
      ;;   ;;                       (cdr (assq :colnames params)))
      ;;   ;;  (org-babel-pick-name (cdr (assq :rowname-names params))
      ;;   ;;                       (cdr (assq :rownames params))))

      ;;   result
      ;;   )
      )))

(when (ignore-errors (require 'counsel))
  (counsel-mode))

(define-minor-mode si-mode
  "testing selfinsert???")

(setf si-mode-map (let ((map (make-sparse-keymap)))
		    (define-key map [remap self-insert-command]
				(lambda () (let ((this (this-command-keys-vector)))
					     (message (prin1-to-string this))
					     this)))
		    map))

(when (ignore-errors (require 'ivy))
  (ivy-mode 1)
  (let ((fn (lambda (a b)
	      (< (length (if (consp a) (car a) a))
		 (length (if (consp b) (car b) b))))))
    (ivy-configure 'read-file-name-internal
      :sort-fn fn)
    (ivy-configure 'execute-extended-command
      :sort-fn fn)))

(when (and (ignore-errors (require 'calendar))
	   (ignore-errors (require 'treepy)))
  (eval (treepy-prewalk-replace '((mode-line-format . header-line-format))
				(func-def 'calendar-update-mode-line))))

(when (ignore-errors (require 'repeat))
  (repeat-mode 1)
  (defvar-keymap lisp-traverse-sexp-repeat-map
    :repeat t
    "f" #'forward-sexp
    "C-f" #'forward-sexp
    "b" #'backward-sexp
    "C-b" #'backward-sexp
    "p" #'jepson/up-list
    "C-p" #'jepson/up-list
    "n" #'down-list
    "C-n" #'down-list))

(when (ignore-errors (require 'simple-httpd)))

(when (ignore-errors (and (require 'titan-mode)
			  (not (null (getenv "TITAN")))))
  (setq-default mode-line-format
		'((:eval
		   (propertize (format " %s%s%s%s%s%s%s "
				       (let ((mod (alist-get 'meta titan-mode--modifiers)))
					 (or (nth 2 (when (member (car mod) titan-mode--current-modifiers)
						      mod))
					     ""))
				       (let ((mod (alist-get 'control titan-mode--modifiers)))
					 (or (nth 2 (when (member (car mod) titan-mode--current-modifiers)
						      mod))
					     ""))
				       (let ((mod (alist-get 'shift titan-mode--modifiers)))
					 (or (nth 2 (when (member (car mod) titan-mode--current-modifiers)
						      mod))
					     ""))
				       (let ((mod (alist-get 'hyper titan-mode--modifiers)))
					 (or (nth 2 (when (member (car mod) titan-mode--current-modifiers)
						      mod))
					     ""))
				       (let ((mod (alist-get 'super titan-mode--modifiers)))
					 (or (nth 2 (when (member (car mod) titan-mode--current-modifiers)
						      mod))
					     ""))
				       (let ((mod (alist-get 'alt titan-mode--modifiers)))
					 (or (nth 2 (when (member (car mod) titan-mode--current-modifiers)
						      mod))
					     ""))
				       (if (eq nil titan-mode--current-modifiers) "-" ""))
			       'face `((t (:background ,(if titan-mode
							    "#0F0"
							  "#F00")))))
		   )
		  "%e"
		  mode-line-front-space
		  (:propertize
		   ("" mode-line-mule-info mode-line-client mode-line-modified
		    mode-line-remote mode-line-window-dedicated)
		   display
		   (min-width (6.0)))
		  mode-line-frame-identification
		  mode-line-buffer-identification
		  "   "
		  mode-line-position
		  (project-mode-line project-mode-line-format)
		  (vc-mode vc-mode)
		  "  "
		  mode-line-modes
		  mode-line-misc-info
		  mode-line-end-spaces)))

(provide 'config)
