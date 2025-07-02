;;; -*- lexical-binding: t -*-

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
 '(browse-url-browser-function 'eww-browse-url)
 '(completion-pcm-leading-wildcard t)
 '(completion-pcm-word-delimiters " -_./:| ")
 '(custom-enabled-themes '(tango))
 '(custom-safe-themes
   '("1781e8bccbd8869472c09b744899ff4174d23e4f7517b8a6c721100288311fa5"
     "de8f2d8b64627535871495d6fe65b7d0070c4a1eb51550ce258cd240ff9394b0"
     "e7820b899036ae7e966dcaaec29fd6b87aef253748b7de09e74fdc54407a7a02"
     default))
 '(dired-dwim-target 'dired-dwim-target-recent)
 '(dired-isearch-filenames t)
 '(display-buffer-alist '(("*Buffer List*" display-buffer-same-window (nil))))
 '(dynamic-completion-mode t)
 '(electric-pair-mode t)
 '(frame-resize-pixelwise t)
 '(geiser-guile-binary "guile2.2")
 '(indent-tabs-mode nil)
 '(isearch-wrap-pause 'no)
 '(magit-clone-always-transient t)
 '(menu-bar-mode nil)
 '(mini-modeline-echo-duration 10)
 '(mini-modeline-face-attr '(:background "rey75"))
 '(org-babel-J-command "~/.nix-profile/bin/jconsole")
 '(org-format-latex-options
   '(:foreground default :background default :scale 4.0 :html-foreground
                 "Black" :html-background "Transparent" :html-scale
                 1.0 :matchers ("begin" "$1" "$" "$$" "\\(" "\\[")))
 '(org-hide-emphasis-markers t)
 '(overriding-text-conversion-style nil t)
 '(package-selected-packages
   '(ada-mode ada-ts-mode auctex avy chemtable consult crux d-mode dash
              elixir-mode elnode erlang evil expand-region
              geiser-guile glsl-mode gnuplot gnuplot-mode god-mode
              haskell-mode ivy j-mode magit mini-modeline minimap
              nix-mode ob-J ob-ada-spark package-lint paredit
              pdf-tools qrencode raku-mode rust-mode scala-mode sly
              smalltalk-mode steamacs system-packages titan-mode tramp
              typescript-mode typetest valign vertico which-key))
 '(package-vc-selected-packages
   '((titan-mode :url "https://github.com/jepsoniada/titan-mode.git")
     (typetest :url "https://github.com/jepsoniada/typetest.git")
     (ob-J :url "https://github.com/jepsoniada/ob-J.git")
     (steamacs :vc-backend Git :url
               "https://github.com/jepsoniada/steamacs.git")))
 '(read-file-name-completion-ignore-case t)
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil)
 '(touch-screen-display-keyboard t)
 '(warning-minimum-level :emergency))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(require 'config)
