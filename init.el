(add-to-list 'load-path "~/.config/emacs/modules")

;;; melpa (bleading-edge)
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

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
;; (defun +keyboard-escape-quit-adv (fun)
;;   "Around advice for `keyboard-escape-quit' FUN. Preserve window configuration when pressing ESC."
;;   (let ((buffer-quit-function (or buffer-quit-function #'ignore)))
;;     (funcall fun)))
;; (advice-add #'keyboard-escape-quit :around #'+keyboard-escape-quit-adv)

;;; features
(put 'set-goal-column 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(tsdh-light))
 '(display-buffer-alist '(("*Buffer List*" display-buffer-same-window (nil))))
 '(geiser-guile-binary "guile2.2")
 '(menu-bar-mode nil)
 '(mini-modeline-echo-duration 10)
 '(mini-modeline-face-attr '(:background "rey75"))
 '(org-babel-J-command "~/.local/bin/jconsole")
 '(org-format-latex-options
   '(:foreground default :background default :scale 4.0 :html-foreground "Black" :html-background "Transparent" :html-scale 1.0 :matchers
		 ("begin" "$1" "$" "$$" "\\(" "\\[")))
 '(org-hide-emphasis-markers t)
 '(package-selected-packages
   '(j-mode gnuplot-mode gnuplot org-contrib magit rust-mode sly glsl-mode scala-mode d-mode tramp god-mode geiser-guile monkeytype evil mini-modeline which-key))
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(setq frame-resize-pixelwise t
      browse-url-browser-function #'eww-browse-url)
(electric-pair-mode t)

(require 'typetest)

;;; god mode
(require 'god-mode)
(require 'god-mode-isearch)

(god-mode)
(setq god-exempt-major-modes nil
      god-exempt-predicates nil)
;; (define-key input-decode-map (kbd "C-i") (kbd "<C-i>"))
(global-set-key (kbd "C-z") #'god-local-mode)

(define-key god-local-mode-map (kbd ".") #'repeat)
(define-key god-local-mode-map (kbd "z") #'god-local-mode)

(define-key isearch-mode-map (kbd "C-z") 'god-mode-isearch-activate)
(define-key god-mode-isearch-map (kbd "z") 'god-mode-isearch-disable)
(define-key god-mode-isearch-map (kbd "<escape>") 'god-mode-isearch-disable)
;;; org mode
(require 'org)

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
      org-startup-indented t)

(require 'tramp)

(require 'tetris)

(require 'ob-J)

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
    ))
