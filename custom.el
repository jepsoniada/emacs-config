;;; -*- lexical-binding: t -*-

;;; features
(put 'set-goal-column 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(backup-directory-alist '(("." . "~/.config/emacs/backups")))
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
 '(enable-recursive-minibuffers t)
 '(frame-resize-pixelwise t)
 '(geiser-guile-binary "guile2.2")
 '(indent-tabs-mode nil)
 '(isearch-wrap-pause 'no)
 '(magit-clone-always-transient t)
 '(menu-bar-mode nil)
 '(mini-modeline-echo-duration 10)
 '(mini-modeline-face-attr '(:background "rey75"))
 '(minibuffer-depth-indicate-mode t)
 '(org-agenda-files '("~/org/notes.org"))
 '(org-babel-J-command "~/.nix-profile/bin/jconsole")
 '(org-format-latex-options
   '(:foreground default :background default :scale 4.0 :html-foreground
                 "Black" :html-background "Transparent" :html-scale
                 1.0 :matchers ("begin" "$1" "$" "$$" "\\(" "\\[")))
 '(org-hide-emphasis-markers t)
 '(overriding-text-conversion-style nil t)
 '(package-selected-packages
   '(ada-mode ada-ts-mode aggressive-indent auctex avy chemtable consult
              crux d-mode dash elixir-mode ellama elnode erlang
              eshell-syntax-highlighting evil expand-region
              dockerfile-mode geiser-guile glsl-mode gnuplot
              gnuplot-mode go-mode god-mode haskell-mode ivy j-mode
              javap-mode kotlin-mode line-mark magit markdown-mode
              mini-modeline minimap multiple-cursors nix-mode ob-J
              ob-ada-spark package-lint paredit pdf-tools py-preformat
              qrencode raku-mode rust-mode scala-mode selected sly
              smalltalk-mode steamacs system-packages titan-mode tramp
              treepy typescript-mode typetest valign vertico which-key
              yaml yaml-mode marginalia))
 '(package-vc-selected-packages
   '((line-mark :url "https://github.com/jepsoniada/line-mark.git")
     (treepy :url "https://github.com/jepsoniada/treepy.el.git")
     (titan-mode :url "https://github.com/jepsoniada/titan-mode.git")
     (typetest :url "https://github.com/jepsoniada/typetest.git")
     (ob-J :url "https://github.com/jepsoniada/ob-J.git")
     (steamacs :vc-backend Git :url
               "https://github.com/jepsoniada/steamacs.git")))
 '(page-delimiter "\14")
 '(read-file-name-completion-ignore-case t)
 '(scroll-bar-mode nil)
 '(search-exit-option 'edit)
 '(tool-bar-mode nil)
 '(touch-screen-display-keyboard t)
 '(warning-minimum-level :emergency))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
