;;; -*- lexical-binding: t -*-

(setf custom-file (file-name-concat user-emacs-directory "custom.el"))
(load custom-file)

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

(defmacro |> (&rest funcs)
  (cl-loop for func in (reverse funcs)
	   
	   if (and (listp func)
		   (length> func 0))
	   for result = `(let ((% ,func)) %)
	   then `(let ((% ,func)) ,result)
	   
	   else if (null func)
	   return (error "empty function")
	   
	   else if (symbolp func)
	   for result = `(let ((% (,func %))) %)
	   then `(let ((% (,func %))) ,result)
	   
	   else
	   return (error "not implemented")
	   
	   finally return `(lambda (%) ,result)))

(add-to-list 'window-state-change-hook
	     (lambda nil
	       (set-register ?@ (buffer-file-name (current-buffer)))))

(define-minor-mode just-compile-mode
  ""
  :keymap (define-keymap
	    "C-c C-c" (lambda (&optional arg) (interactive "p")
			(if (= arg 1)
			  (call-interactively #'recompile)
			  (call-interactively #'compile)))))

(defun jepson/make-ivy-pattern (pattern)
  (mapcan (lambda (elem)
	    (if (stringp elem)
	      (seq-map (lambda (char)
			 (if (eq char ? )
			   'any
			   (string char)))
		       elem)))
	  pattern))
(defun completion-ivy-try-completion (string table pred point)
  (pcase-let ((`(,all ,pattern ,prefix ,suffix ,_carbounds)
	       (completion-substring--all-completions
                string table pred point
                #'jepson/make-ivy-pattern)))
    (if minibuffer-completing-file-name
      (setq all (completion-pcm--filename-try-filter all)))
    (completion-pcm--merge-try pattern all prefix suffix)))

(defun completion-ivy-all-completions (string table pred point)
  (pcase-let ((`(,all ,pattern ,prefix ,_suffix ,_carbounds)
	       (completion-substring--all-completions
                string table pred point
                #'jepson/make-ivy-pattern)))
    (when all
      (nconc (completion-pcm--hilit-commonality pattern all)
             (length prefix)))))

(define-minor-mode project-mode
  "do your job NOW"
  :keymap (define-keymap
	    "<remap> <find-file>"                #'project-find-file
            "C-c C-f"                            #'find-file
            "<remap> <list-buffers>"             #'project-list-buffers
            "C-c C-b"                            #'list-buffers
            "<remap> <async-shell-command>"      #'project-async-shell-command
            "C-c M-&"                            #'async-shell-command
            "<remap> <switch-to-buffer>"         #'project-switch-to-buffer
            "C-c b"                              #'switch-to-buffer
            "<remap> <dired>"                    #'project-find-dir
            "C-c d"                              #'dired
            "C-c e"                              #'project-eshell
            "<remap> <execute-extended-command>" #'project-execute-extended-command
            "C-c M-x"                            #'execute-extended-command
            "<remap> <save-some-buffers>"        #'project-save-some-buffers
            "C-c s"                              #'save-some-buffers))

(defun highlight-region (start end color)
  (interactive (list (region-beginning) (region-end) (read-color)))
  (put-text-property start end 'face (list :background color)))

(defun multiline (&rest lines)
  (mapconcat #'identity
             lines
             "\n"))

;;; completion

(setf (alist-get 'ivy completion-styles-alist)
      '(completion-ivy-try-completion completion-ivy-all-completions
				      "ivy like completion"))

(setf completion-styles '(ivy partial-completion emacs22))

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

(defun jepson/up-list (&rest _)
  (interactive)
  (pcase-let* ((`(_ _ _ ,strp _ _ _ _ ,left-outer) (syntax-ppss)))
    (if strp
      (goto-char left-outer)
      (progn (up-list) (backward-sexp)))))

(defun jepson/backward-sexp (&optional arg interactive)
  (interactive "^p\nd")
  (pcase-let* ((`(_ _ _ ,strp _ _ _ _ ,left-outer) (syntax-ppss))
               (left-outer (when left-outer
                             (save-excursion (goto-char left-outer)
                                             (forward-char)
                                             (point)))))
    (if strp
      (when (>= (scan-sexps (point) (- (or arg 1))) left-outer)
        (goto-char left-outer))
      (backward-sexp arg interactive))))

(defun jepson/forward-sexp (&optional arg interactive)
  (interactive "^p\nd")
  (pcase-let* ((`(_ _ _ ,strp _ _ _ _ ,left-outer) (syntax-ppss))
               (right-outer (when left-outer
                              (save-excursion (goto-char left-outer)
                                              (forward-sexp arg interactive)
                                              (backward-char)
                                              (point)))))
    (if strp
      (when (>= (scan-sexps (point) (or arg 1)) (or right-outer 0))
        (goto-char right-outer))
      (forward-sexp arg interactive))))

(keymap-global-set "C-M-n" #'down-list)
(keymap-global-set "C-M-p" #'jepson/up-list)
(keymap-global-set "C-M-f" #'jepson/forward-sexp)
(keymap-global-set "C-M-b" #'jepson/backward-sexp)

(keymap-global-set "M-k" #'kill-whole-line)

(keymap-global-set "M-n" #'minibuffer-next-completion)
(keymap-global-set "M-p" #'minibuffer-previous-completion)

(keymap-global-set "C-x t" #'eshell)

(keymap-global-set "C-M-@" #'rectangle-mark-mode)
(keymap-global-unset "M-@")

(keymap-global-set "C-x f"
                   (lambda ()
                     (interactive)
                     (find-file user-init-file)))

(use-package ibuffer
  :bind ( ("C-x C-b" . ibuffer))
  :config
  (defun ibuffer-list-buffer-minor-modes ()
    "list of minor modes for completion"
    (let ((flat-repeating (cl-loop for buf in (buffer-list)
                                   append (buffer-local-value 'local-minor-modes buf))))
      (seq-map #'symbol-name (seq-uniq flat-repeating))))
  (define-ibuffer-filter used-minor-mode
      "ibuffer filter for minor modes"
    ( :description "local minor mode in use"
      :accept-list t
      :reader (let ((buf (ibuffer-current-buffer)))
                (mapcar #'intern
                        (completing-read-multiple
                         (format-prompt "Filter by minor mode" nil)
                         (ibuffer-list-buffer-minor-modes)
                         nil
                         t
                         nil
                         nil
                         nil))))
    (member qualifier (buffer-local-value 'local-minor-modes buf))))

;;; advices

(defun jepson/no-undo (fn &rest rest)
  (with-undo-amalgamate (apply fn rest)))
(advice-add 'apply-macro-to-region-lines :around 'jepson/no-undo)

;;; elisp indent

(put 'if 'lisp-indent-function 1)

;;; loader for custom indentation

(defun jepson/load-indent nil
  (interactive)
  (let* ((indent-dir (file-name-concat (file-name-directory user-init-file) "indent"))
         (files (directory-files indent-dir))
         (name (symbol-name major-mode)))
    (ignore-error file-error
      (load-file (file-name-concat indent-dir
                                   (concat (substring name
                                                      0
                                                      (string-search "-mode" name))
                                           ".indent.el"))))))

(add-hook 'prog-mode-hook (lambda nil
                            (let ((hook (intern (concat (symbol-name major-mode) "-hook"))))
                              (add-hook hook #'jepson/load-indent))))

;;; org mode
(when (ignore-errors (require 'org))

  (dolist (face '((org-level-1 . 2.0)
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
	org-startup-with-inline-images t
	org-default-notes-file (concat org-directory "/notes.org"))

  (add-to-list 'org-mode-hook
	       (lambda nil
		 (visual-line-mode 1)))

  (define-key org-mode-map (kbd "C-<") #'org-metaleft)
  (define-key org-mode-map (kbd "C->") #'org-metaright)

  (keymap-unset org-mode-map "M-h")

  (when (ignore-errors (require 'valign))
    (add-hook 'org-mode-hook #'valign-mode)))

(when (ignore-errors (require 'dired))
  (setf dired-listing-switches "-hAl")

  (defun jepson/dired-mark  (arg &optional interactive)
    (interactive (list current-prefix-arg t) dired-mode)
    (dired-mark arg (when interactive interactive)))
  (defun jepson/dired-unmark (arg &optional interactive)
    (interactive (list current-prefix-arg t) dired-mode)
    (dired-unmark arg (when interactive interactive)))
  (defun jepson/dired-unmark-all-marks ()
    (interactive nil dired-mode)
    (dired-unmark-all-marks))
  
  (keymap-set dired-mode-map "m" #'jepson/dired-mark)
  (keymap-set dired-mode-map "u" #'jepson/dired-unmark)
  (keymap-set dired-mode-map "U" #'jepson/dired-unmark-all-marks)
  (keymap-set dired-mode-map "* n" #'dired-next-marked-file)
  (keymap-set dired-mode-map "* p" #'dired-prev-marked-file)
  (keymap-set dired-mode-map "* r" #'dired-mark-files-regexp)
  (keymap-set dired-mode-map "TAB" #'dired-hide-subdir)
  (keymap-set dired-mode-map "]" #'dired-next-subdir)
  (keymap-set dired-mode-map "[" #'dired-prev-subdir)

  (defvar-keymap dired-subdir-repeat-map
    :repeat t
    "n" #'dired-next-subdir
    "p" #'dired-prev-subdir
    "TAB" #'dired-hide-subdir
    "s" #'dired-mark-subdir-files)
  (defvar-keymap dired-mark-repeat-map
    :repeat t
    "n" #'dired-next-marked-file
    "p" #'dired-prev-marked-file
    "U" #'dired-unmark-all-marks
    "%" #'dired-mark-files-regexp
    "r" #'dired-mark-files-regexp
    "*" #'dired-mark-executables
    "/" #'dired-mark-directories
    "@" #'dired-mark-symlinks
    "N" #'dired-number-of-marked-files
    "c" #'dired-change-marks
    "m" #'dired-mark
    "s" #'dired-mark-subdir-files
    "t" #'dired-toggle-marks
    "u" #'dired-unmark
    "DEL" #'dired-unmark-backward))

(ignore-errors (require 'tramp))

(ignore-errors (require 'typetest))

(use-package avy
  :bind (("C-; C-l" . avy-goto-line)
         ("C-; C-j" . jepson/avy-search))
  :config
  (defun jepson/avy-search ()
    (interactive)
    (let ((avy-timeout-seconds nil))
      (goto-char (car (avy-process (avy--read-candidates)))))))

(use-package vertico
  :config
  (vertico-mode 1))

(use-package consult
  :after (vertico)
  :config
  (setf completion-in-region-function #'consult-completion-in-region))

(use-package treepy
  :vc (:url "https://github.com/jepsoniada/treepy.el.git"))

(when (and (ignore-errors (require 'calendar))
	   (ignore-errors (require 'treepy)))
  (eval (treepy-prewalk-replace '((mode-line-format . header-line-format))
				(func-def 'calendar-update-mode-line))))

(when (ignore-errors (require 'repeat))
  (repeat-mode 1)
  (defvar-keymap lisp-traverse-sexp-repeat-map
    :repeat t
    "f" #'jepson/forward-sexp
    "C-f" #'jepson/forward-sexp
    "b" #'jepson/backward-sexp
    "C-b" #'jepson/backward-sexp
    "p" #'jepson/up-list
    "C-p" #'jepson/up-list
    "n" #'down-list
    "C-n" #'down-list))

(when (ignore-errors (require 'simple-httpd)))

(use-package paredit
  :ensure t
  :pin melpa
  :bind ( :map paredit-mode-map
          ("C-j" . nil)
          ("C-M-b" . nil)
          ("C-M-d" . nil)
          ("C-M-f" . nil)
          ("C-M-n" . nil)
          ("C-M-p" . nil)
          ("C-M-u" . nil)
          ("M-s" . nil)
          ("M-?" . nil))
  :hook lisp-data-mode)

(when (ignore-errors (require 'find-dired))
  (setf find-ls-option `("-exec ls -ldh --quoting-style=literal {} +" . "-ldh --quoting-style=literal")))

(when (ignore-errors (require 'line-mark)))

(use-package nano-theme
  :ensure nil
  :defer t)

(use-package transient
  :bind ( :map transient-map
          ("<escape>" . transient-quit-all)))

(use-package expand-region
  :bind (("M-h"          . nil)
         ("M-h C-d"      . er/mark-defun)
         ("M-h C-i C-\"" . er/mark-inside-quotes)
         ("M-h C-i C-'"  . er/mark-inside-quotes)
         ("M-h C-i C-p"  . er/mark-inside-pairs)
         ("M-h C-a C-\"" . er/mark-outside-quotes)
         ("M-h C-a C-'"  . er/mark-outside-quotes)
         ("M-h C-a C-p"  . er/mark-outside-pairs)
         ("M-h C-c"      . er/mark-comment)
         ("M-h C-s"      . er/mark-symbol)))

(when (ignore-errors (require 'replace)) ;; occur realy
  (defun jepson/occur-change-regexp (regexp &optional nlines)
    (interactive (occur-read-primary-args))
    (setf (car occur-revert-arguments) regexp)
    (revert-buffer))
  (defun jepson/occur-kill-occurrence ()
    (interactive)
    (with-current-buffer (current-buffer)
      (let ((edge-p nil))
        (if edge-p
          (end-of-line)
          (condition-case nil
              (progn
                (next-line)
                (beginning-of-line))
            (end-of-buffer (progn
                             (end-of-line)
                             (setf edge-p t)))))
        (push-mark (point) t t)
        (if edge-p
          (beginning-of-line)
          (condition-case nil
              (previous-line)
            (beginning-of-buffer (progn
                                   (beginning-of-line)
                                   (setf edge-p t))))))
      (facemenu-remove-special (point) (mark))
      (delete-region (point) (mark))))
  (keymap-set occur-mode-map "r" #'jepson/occur-change-regexp)
  (keymap-set occur-mode-map "k" #'jepson/occur-kill-occurrence))

(use-package aggressive-indent
  :ensure t
  :pin melpa
  :hook
  (prog-mode . aggressive-indent-mode))

(use-package line-mark
  :vc (:url "https://github.com/jepsoniada/line-mark.git")
  :bind (("M-@" . line-mark-mode)))

(use-package isearch
  :bind ( :map isearch-mode-map
          ("<escape>" . isearch-cancel)
          ("C-g" . isearch-cancel)))

(use-package page
  :config
  (let ((forward (lambda (fn &rest args)
                   (if (buffer-narrowed-p)
                     (progn
                       (widen)
                       (apply fn args)
                       (narrow-to-page))
                     (apply fn args))))
        (backward (lambda (fn &rest args)
                    (if (buffer-narrowed-p)
                      (progn
                        (widen)
                        (apply fn args)
                        (apply fn args)
                        (narrow-to-page))
                      (apply fn args)))))
    (advice-add #'forward-page
                :around
                forward)
    (advice-add #'backward-page
                :around
                backward)))

(use-package winner
  :demand t
  :bind (("C-x w /" . winner-undo)
         ("C-x w ?" . winner-redo))
  :bind (:repeat-map winner-mode-repeat-map
                     ("/" . winner-undo)
                     ("?" . winner-redo))
  :config
  (winner-mode 1))

(use-package marginalia
  :config
  (marginalia-mode 1))

;;; god mode
(use-package god-mode
  :after (paredit)
  :ensure t
  :pin melpa
  :config
  (when (null (getenv "TITAN"))
    (add-to-list 'window-buffer-change-functions
                 (lambda (window-or-frame)
                   (when (not (minibufferp))
	             (god-local-mode 1))))
    (add-to-list 'window-selection-change-functions
	         (lambda (window-or-frame)
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

    (when (ignore-errors (require 'god-mode-isearch))
      (define-key isearch-mode-map (kbd "C-z") 'god-mode-isearch-activate)
      (define-key isearch-mode-map (kbd "π") 'god-mode-isearch-activate)
      (define-key god-mode-isearch-map (kbd "z") 'god-mode-isearch-disable)
      (define-key god-mode-isearch-map (kbd "<escape>") 'god-mode-isearch-disable))))

(when (ignore-errors (and (require 'titan-mode)
			  (not (null (getenv "TITAN")))))
  (keymap-set isearch-mode-map "<escape>" 'titan-mode-change-modifiers)
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

(defun eshell/comint-run (program &rest args)
  (comint-run program args))

(provide 'config)
