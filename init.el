(add-to-list 'load-path "/home/jepson/.config/emacs/modules")

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
 '(custom-enabled-themes '(wombat))
 '(display-buffer-alist '(("*Buffer List*" display-buffer-same-window (nil))))
 '(geiser-guile-binary "guile2.2")
 '(menu-bar-mode nil)
 '(mini-modeline-echo-duration 999)
 '(mini-modeline-echo-position "left")
 '(mini-modeline-face-attr '(:background "rey75"))
 '(mini-modeline-l-format nil)
 '(org-format-latex-options
   '(:foreground default :background default :scale 4.0 :html-foreground "Black" :html-background "Transparent" :html-scale 1.0 :matchers
		 ("begin" "$1" "$" "$$" "\\(" "\\[")))
 '(org-hide-emphasis-markers t)
 '(package-selected-packages
   '(magit rust-mode sly glsl-mode scala-mode d-mode tramp use-package god-mode geiser-guile monkeytype evil mini-modeline which-key))
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

(use-package typetest)

;;; god mode
(use-package god-mode
  :init
  (god-mode)
  (setq god-exempt-major-modes nil
	god-exempt-predicates nil)
  ;; (define-key input-decode-map (kbd "C-i") (kbd "<C-i>"))
  (global-set-key (kbd "C-z") #'god-local-mode)
  :config
  (define-key god-local-mode-map (kbd ".") #'repeat)
  (define-key god-local-mode-map (kbd "z") #'god-local-mode)
  )

;;; org mode
(use-package org
  :config
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
	org-startup-indented t))

(use-package tramp)

(use-package tetris)
