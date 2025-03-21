(add-to-list 'load-path "~/.config/emacs/modules")

;;; features
(put 'set-goal-column 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(completion-pcm-leading-wildcard t)
 '(completion-pcm-word-delimiters " -_./:| ")
 '(custom-enabled-themes '(tango))
 '(custom-safe-themes
   '("1781e8bccbd8869472c09b744899ff4174d23e4f7517b8a6c721100288311fa5"
     "de8f2d8b64627535871495d6fe65b7d0070c4a1eb51550ce258cd240ff9394b0"
     "e7820b899036ae7e966dcaaec29fd6b87aef253748b7de09e74fdc54407a7a02"
     default))
 '(display-buffer-alist '(("*Buffer List*" display-buffer-same-window (nil))))
 '(geiser-guile-binary "guile2.2")
 '(menu-bar-mode nil)
 '(mini-modeline-echo-duration 10)
 '(mini-modeline-face-attr '(:background "rey75"))
 '(org-babel-J-command "~/.local/bin/jconsole")
 '(org-format-latex-options
   '(:foreground default :background default :scale 4.0 :html-foreground
		 "Black" :html-background "Transparent" :html-scale
		 1.0 :matchers ("begin" "$1" "$" "$$" "\\(" "\\[")))
 '(org-hide-emphasis-markers t)
 '(package-selected-packages
   '(ada-mode ada-ts-mode auctex avy chemtable d-mode dash elnode evil
	      geiser-guile glsl-mode gnuplot gnuplot-mode god-mode ivy
	      j-mode magit mini-modeline minimap monkeytype nano-theme
	      ob-J ob-ada-spark package-lint paredit pdf-tools
	      qrencode rust-mode scala-mode sly smalltalk-mode
	      system-packages titan-mode tramp typetest valign vertico
	      which-key))
 '(package-vc-selected-packages
   '((ob-J :url "https://github.com/jepsoniada/ob-J.git")
     (typetest :url "https://github.com/jepsoniada/typetest.git")
     (titan-mode :url "https://github.com/jepsoniada/titan-mode.git")))
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(require 'config)
