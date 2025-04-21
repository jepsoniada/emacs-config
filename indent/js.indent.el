(defun js--proper-indentation (parse-status)
  "Return the proper indentation for the current line."
  (save-excursion
    (back-to-indentation)
    (cond ((nth 4 parse-status)    ; inside comment
           (js--get-c-offset 'c (nth 8 parse-status)))
          ((nth 3 parse-status) 0) ; inside string
          ((when (and js-jsx-syntax (not js-jsx--indent-col))
             (save-excursion (js-jsx--indentation parse-status))))
          ((and (eq (char-after) ?#)
                (save-excursion
                  (forward-char 1)
                  (looking-at-p cpp-font-lock-keywords-source-directives)))
           0)
          ((save-excursion (js--beginning-of-macro)) 4)
          ;; Indent array comprehension continuation lines specially.
          ((let ((bracket (nth 1 parse-status))
                 beg)
             (and bracket
                  (not (js--same-line bracket))
                  (setq beg (js--indent-in-array-comp bracket))
                  ;; At or after the first loop?
                  (>= (point) beg)
                  (js--array-comp-indentation bracket beg))))
          ((js--chained-expression-p))
          ((js--ctrl-statement-indentation))
          ((js--multi-line-declaration-indentation))
          ((nth 1 parse-status)
	   ;; A single closing paren/bracket should be indented at the
	   ;; same level as the opening statement. Same goes for
	   ;; "case" and "default".
           (let ((same-indent-p (looking-at "[]})]"))
                 (switch-keyword-p (looking-at "default\\_>\\|case\\_>[^:]"))
                 (continued-expr-p (js--continued-expression-p)))
             (goto-char (nth 1 parse-status)) ; go to the opening char
             (if (or (not js-indent-align-list-continuation)
                     (looking-at "[({[]\\s-*\\(/[/*]\\|$\\)")
                     (save-excursion (forward-char) (js--broken-arrow-terminates-line-p)))
               (progn ; nothing following the opening paren/bracket
                 (skip-syntax-backward " ")
                 (when (eq (char-before) ?\)) (backward-list))
                 (back-to-indentation)
                 (when (eq (char-after) ?,)
                   (forward-char)
                   (skip-chars-forward " \t"))
                 (js--maybe-goto-declaration-keyword-end parse-status)
                 (let* ((in-switch-p (unless same-indent-p
                                       (looking-at "\\_<switch\\_>")))
                        (same-indent-p (or same-indent-p
                                           (and switch-keyword-p
                                                in-switch-p)))
                        (indent
                         (+
                          (cond
                           ((and js-jsx--indent-attribute-line
                                 (eq js-jsx--indent-attribute-line
                                     (line-number-at-pos)))
                            js-jsx--indent-col)
                           (t
                            (current-column)))
                          (cond (same-indent-p 0)
                                (continued-expr-p
                                 (+ (* 2 js-indent-level)
                                    js-expr-indent-offset))
                                (t
                                 (+ js-indent-level
                                    (pcase (char-after (nth 1 parse-status))
                                      (?\( js-paren-indent-offset)
                                      (?\[ js-square-indent-offset)
                                      (?\{ js-curly-indent-offset))))))))
                   (if in-switch-p
                     (+ indent js-switch-indent-offset)
                     indent)))
               (current-column))))

          ((js--continued-expression-p)
           (+ js-indent-level js-expr-indent-offset))
          (t (prog-first-column)))))
