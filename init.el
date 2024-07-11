(add-to-list 'load-path "~/.config/emacs/modules")
(require 'config)

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
   '(ivy avy d-mode dash evil geiser-guile glsl-mode gnuplot gnuplot-mode god-mode j-mode magit mini-modeline monkeytype org-contrib paredit rust-mode scala-mode sly smalltalk-mode titan-mode tramp typetest which-key))
 '(package-vc-selected-packages
   '((typetest :url "https://github.com/jepsoniada/typetest.git")
     (titan-mode :url "https://github.com/jepsoniada/titan-mode.git")))
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
