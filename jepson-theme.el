(deftheme jepson
  "Created 2026-05-28."
  :background-mode 'light
  :kind 'color-scheme
  :family 'jepson)

(let ((class '((class color) (type graphic)))
      (tty '((class color) (type tty) (min-colors 8)))

      (butter-1 "#fce94f") (butter-2 "#edd400") (butter-3 "#c4a000")
      (orange-1 "#fcaf3e") (orange-2 "#f57900") (orange-3 "#ce5c00")
      (choc-1 "#e9b96e") (choc-2 "#c17d11") (choc-3 "#8f5902")
      (cham-1 "#8ae234") (cham-2 "#73d216") (cham-3 "#4e9a06")
      (blue-1 "#729fcf") (blue-2 "#3465a4") (blue-3 "#204a87")
      (plum-1 "#ad7fa8") (plum-2 "#75507b") (plum-3 "#5c3566")
      (red-1 "#ef2929")  (red-2 "#cc0000")  (red-3 "#a40000")
      (alum-1 "#eeeeec") (alum-2 "#d3d7cf") (alum-3 "#babdb6")
      (alum-4 "#888a85") (alum-5 "#5f615c") (alum-6 "#2e3436")
      ;; Not in Tango palette; used for better contrast.
      (cham-4 "#346604") (blue-0 "#8cc4ff") (orange-4 "#b35000")
      ;; Not in Tango palette; used for ANSI cyan.
      (cyan-1 "#34e2e2") (cyan-2 "#06989a"))

  (custom-theme-set-faces
   'jepson
   `(default                                       ((,class
                                                     ( :family "monospace"
                                                       :width normal
                                                       :weight regular
                                                       :slant normal
                                                       :foreground ,alum-6
                                                       :background ,alum-1))
                                                    (t
                                                     ( :family "monospace"
                                                       :width normal
                                                       :weight regular
                                                       :slant normal
                                                       :foreground "white"
                                                       :background "black"))))
   `(cursor                                        ((t
                                                     ( :inverse-video t))))
   `(fringe                                        ((,class
                                                     ( :background ,alum-2))
                                                    (t
                                                     ( :background "gray"))))
   `(highlight                                     ((,class
                                                     ( :background ,alum-3))
                                                    (t
                                                     ( :background "blue"))))
   `(region                                        ((,class
                                                     ( :background ,alum-3))
                                                    (t
                                                     ( :foreground "white"
                                                       :background "blue"))))
   `(secondary-selection                           ((,class
                                                     ( :background ,blue-0))
                                                    (t
                                                     ( :foreground "black" 
                                                       :background "cyan"))))
   `(isearch                                       ((,class
                                                     ( :foreground "#ffffff" 
                                                       :background ,orange-3))
                                                    (,tty
                                                     ( :foreground "cyan" 
                                                       :background "magenta"))
                                                    (t
                                                     ( :inverse-video t))))
   `(lazy-highlight                                ((,class
                                                     ( :background ,choc-1))
                                                    (t
                                                     ( :foreground "black" 
                                                       :background "cyan"))))
   `(trailing-whitespace                           ((,class
                                                     ( :background ,red-1))
                                                    (,tty
                                                     ( :background "red"))
                                                    (t
                                                     ( :inverse-video t))))
   `(mode-line                                     ((,class
                                                     ( :box (:line-width -1 :style released-button)
			                               :background ,alum-2
                                                       :foreground ,alum-6))
                                                    (t
                                                     ( :inverse-video t))))
   `(mode-line-inactive                            ((,class
                                                     ( :inherit (mode-line)
				                       :background ,alum-4))
                                                    (t
                                                     ( :inherit (mode-line)))))
   `(minibuffer-prompt                             ((,class
                                                     ( :weight bold 
                                                       :foreground ,blue-3))
                                                    (,tty
                                                     ( :weight bold 
                                                       :foreground "blue"))))
   `(escape-glyph                                  ((,class
                                                     ( :foreground ,red-3))
                                                    (,tty
                                                     ( :foreground "magenta"))))
   `(homoglyph                                     ((t
                                                     ( :inherit (escape-glyph)))))
   `(error                                         ((,class
                                                     ( :foreground ,red-3))
                                                    (,tty
                                                     ( :foreground "red"))))
   `(warning                                       ((,class
                                                     ( :foreground ,orange-3))
                                                    (,tty
                                                     ( :foreground "yellow"))))
   `(success                                       ((,class
                                                     ( :foreground ,cham-3))
                                                    (,tty
                                                     ( :foreground "green"))))
   `(font-lock-builtin-face                        ((,class
                                                     ( :foreground ,plum-2))
                                                    (,tty
                                                     ( :foreground "magenta"))))
   `(font-lock-comment-face                        ((,class
                                                     ( :slant italic
                                                       :foreground ,alum-5))
                                                    (,tty
                                                     ( :foreground "yellow"))))
   `(font-lock-comment-delimiter-face              ((t
                                                     ( :inherit (font-lock-comment-face)))))
   `(font-lock-constant-face                       ((,class
                                                     ( :weight bold
                                                       :foreground ,blue-3))
                                                    (,tty
                                                     ( :width bold
                                                       :foreground "blue"))))
   `(font-lock-function-name-face                  ((,class
                                                     ( :foreground ,red-3))
                                                    (,tty
                                                     ( :foreground "red"))))
   `(font-lock-function-call-face                  ((t
                                                     ( :inherit (font-lock-function-name-face)))))
   `(font-lock-keyword-face                        ((,class
                                                     ( :foreground ,cham-4))
                                                    (,tty
                                                     ( :weight bold 
                                                       :foreground "cyan"))))
   `(font-lock-string-face                         ((,class
                                                     ( :foreground ,plum-3))
                                                    (,tty
                                                     ( :foreground "magenta"))))
   `(font-lock-type-face                           ((,class
                                                     ( :foreground ,blue-3))
                                                    (,tty
                                                     ( :weight bold
                                                       :foreground "cyan"))))
   `(font-lock-variable-name-face                  ((,class
                                                     ( :foreground ,orange-4))
                                                    (,tty
                                                     ( :foreground "yellow"))))
   `(font-lock-variable-use-face                   ((t
                                                     ( :inherit (font-lock-variable-name-face)))))
   `(font-lock-bracket-face                        ((t
                                                     ( :inherit (font-lock-punctuation-face)))))
   `(font-lock-delimiter-face                      ((t
                                                     ( :inherit (font-lock-punctuation-face)))))
   `(font-lock-doc-face                            ((t
                                                     ( :inherit (font-lock-string-face)))))
   `(font-lock-doc-markup-face                     ((t
                                                     ( :inherit (font-lock-constant-face)))))
   `(font-lock-escape-face                         ((t
                                                     ( :inherit (font-lock-regexp-grouping-backslash)))))
   `(font-lock-negation-char-face                  ((t
                                                     nil)))
   `(font-lock-number-face                         ((t
                                                     nil)))
   `(font-lock-misc-punctuation-face               ((t
                                                     ( :inherit (font-lock-punctuation-face)))))
   `(font-lock-operator-face                       ((t
                                                     ( :inherit (font-lock-function-call-face)))))
   `(font-lock-preprocessor-face                   ((t
                                                     ( :inherit (font-lock-function-call-face)))))
   `(font-lock-property-name-face                  ((t
                                                     ( :inherit (font-lock-variable-name-face)))))
   `(font-lock-property-use-face                   ((t
                                                     ( :inherit (font-lock-property-name-face)))))
   `(font-lock-punctuation-face                    ((t
                                                     nil)))
   `(font-lock-regexp-grouping-backslash           ((t
                                                     ( :inherit (bold)))))
   `(font-lock-regexp-grouping-construct           ((t
                                                     ( :inherit (bold)))))
   `(link                                          ((,class
                                                     ( :underline t
                                                       :foreground ,blue-3))
                                                    (,tty
                                                     ( :underline ( :color foreground-color :style line :position nil) 
                                                       :foreground "cyan"))
                                                    (t
                                                     ( :inherit (underline)))))
   `(link-visited                                  ((,class
                                                     ( :foreground "magenta"))
                                                    (t
                                                     ( :inherit (link)))))
   `(message-header-name                           ((,class
                                                     ( :foreground ,blue-3))
                                                    (,tty
                                                     ( :weight bold
                                                       :foreground "cyan"))))
   `(message-header-cc                             ((,class
                                                     ( :foreground ,butter-3))
                                                    (,tty
                                                     ( :foreground "yellow"))))
   `(message-header-other                          ((,class
                                                     ( :foreground ,choc-2))
                                                    (,tty
                                                     ( :foreground "yellow"))))
   `(message-header-subject                        ((,class
                                                     ( :foreground ,red-3))
                                                    (,tty
                                                     ( :foreground "red"))))
   `(message-header-to                             ((,class
                                                     ( :weight bold 
                                                       :foreground ,butter-3))
                                                    (,tty
                                                     ( :foreground "yellow"))))
   `(message-cited-text                            ((,class
                                                     ( :slant italic 
                                                       :foreground ,alum-5))))
   `(message-separator                             ((,class
                                                     ( :weight bold 
                                                       :foreground ,cham-3))
                                                    (,tty
                                                     ( :foreground "green"))))
   `(smerge-refined-changed                        ((,class
                                                     ( :background ,plum-1))
                                                    (,tty
                                                     ( :background "magenta"))))
   `(flyspell-duplicate                            ((,class
                                                     ( :underline ,orange-1))
                                                    (,tty
                                                     ( :foreground "yellow"))))
   `(flyspell-incorrect                            ((,class
                                                     ( :underline ,red-1))
                                                    (,tty
                                                     ( :foreground "red"))))
   `(semantic-decoration-on-includes               ((,class 
                                                     ( :underline ,cham-4))))
   `(semantic-decoration-on-private-members-face   ((,class 
                                                     ( :background ,alum-2))))
   `(semantic-decoration-on-protected-members-face ((,class 
                                                     ( :background ,alum-2))))
   `(semantic-decoration-on-unknown-includes       ((,class 
                                                     ( :background ,choc-3))))
   `(semantic-decoration-on-unparsed-includes      ((,class 
                                                     ( :underline ,orange-3))))
   `(semantic-tag-boundary-face                    ((,class 
                                                     ( :overline ,blue-1))))
   `(semantic-unmatched-syntax-face                ((,class 
                                                     ( :underline ,red-1))))
   `(fixed-pitch                                   ((t
                                                     ( :family "monospace"))))
   `(variable-pitch                                ((t
                                                     ( :family "monospace"))))
   `(shadow                                        ((((class color grayscale) (min-colors 88) (background light))
                                                     ( :foreground "grey50"))
                                                    (((class color grayscale) (min-colors 88) (background dark))
                                                     ( :foreground "grey70"))
                                                    (((class color) (min-colors 8) (background light))
                                                     ( :foreground "green"))
                                                    (((class color) (min-colors 8) (background dark))
                                                     ( :foreground "yellow"))))
   `(button                                        ((t
                                                     ( :inherit (link)))))
   `(header-line                                   ((t
                                                     ( :inherit (mode-line)))))
   `(tooltip                                       ((((class color))
                                                     ( :inherit (variable-pitch) 
                                                       :foreground "black" 
                                                       :background "lightyellow"))
                                                    (t
                                                     ( :inherit (variable-pitch)))))
   `(mode-line-buffer-id                           ((t
                                                     ( :weight bold))))
   `(mode-line-emphasis                            ((t
                                                     ( :weight bold))))
   `(mode-line-highlight                           ((((supports :box t) (class color grayscale) (min-colors 88))
                                                     ( :box ( :line-width (2 . 2) :color "grey40" :style released-button)))
                                                    (t
                                                     ( :inherit (highlight)))))
   `(isearch-fail                                  ((((class color) (min-colors 88) (background light))
                                                     ( :background "RosyBrown1"))
                                                    (((class color) (min-colors 88) (background dark))
                                                     ( :background "red4"))
                                                    (((class color) (min-colors 16))
                                                     ( :background "red"))
                                                    (((class color) (min-colors 8))
                                                     ( :background "red"))
                                                    (((class color grayscale))
                                                     ( :foreground "grey"))
                                                    (t
                                                     ( :inverse-video t))))
   `(match                                         ((((class color) (min-colors 88) (background light))
                                                     ( :background "khaki1"))
                                                    (((class color) (min-colors 88) (background dark))
                                                     ( :background "RoyalBlue3"))
                                                    (((class color) (min-colors 8) (background light))
                                                     ( :foreground "black" 
                                                       :background "yellow"))
                                                    (((class color) (min-colors 8) (background dark))
                                                     ( :foreground "white" 
                                                       :background "blue"))
                                                    (((type tty) (class mono))
                                                     ( :inverse-video t))
                                                    (t
                                                     ( :background "gray"))))
   `(next-error                                    ((t
                                                     ( :inherit (region)))))
   `(query-replace                                 ((t
                                                     ( :inherit (isearch)))))
   `(marginalia-file-name                          ((((class color) (min-colors 88))
                                                     ( :inherit (marginalia-file-name)))
                                                    (((class color) (min-colors 8))
                                                     ( :foreground "cyan"))))
   `(marginalia-documentation                      ((((class color) (min-colors 88))
                                                     ( :inherit (marginalia-documentation)))
                                                    (((class color) (min-colors 8))
                                                     ( :foreground "cyan"))))
   `(completions-annotations                       ((t
                                                     ( :inherit (shadow)))))))

(provide-theme 'jepson)
