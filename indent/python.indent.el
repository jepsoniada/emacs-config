(defun python-indent-region (start end)
  (let ((deactivate-mark nil))
    (save-excursion
      (goto-char end)
      (setq end (point-marker))
      (goto-char start)
      (or (bolp) (forward-line 1))
      (while (< (point) end)
        (or (and (bolp) (eolp))
            (when (and
                   ;; Skip if previous line is a comment.
                   (save-excursion
                     (let ((line-is-comment-p
                            (python-info-current-line-comment-p)))
                       (forward-line -1)
                       (not
                        (or (and (python-info-current-line-comment-p)
                                 ;; Unless this line is a comment too.
                                 (not line-is-comment-p))))))
                   ;; Don't mess with strings, unless it's the
                   ;; enclosing set of quotes or a docstring.
                   (or (not (python-syntax-context 'string))
                       (equal
                        (syntax-after
                         (+ (1- (point))
                            (current-indentation)
                            (python-syntax-count-quotes (char-after) (point))))
                        (string-to-syntax "|"))
                       (python-info-docstring-p)))
              (python-indent-line)))
        (forward-line 1))
      (move-marker end nil))))

(defun python-indent-line ;; python-indent-line-function
    (&optional previous)
  (let ((number-or-list (save-excursion
                          (pcase (python-indent-context)
                            (`(:no-indent . ,_) (prog-first-column)) ; usually 0
                            (`(:inside-string . ,_)
                             (current-indentation))
                            (`(,(or :after-line
                                    :after-comment
                                    :after-backslash
                                    :inside-paren-continuation-line) . ,start)
                             ;; Copy previous indentation.
                             (let ((end (point))
                                   (start (goto-char start))
                                   indent
                                   newline-count)
                               (setf indent
                                     (current-indentation)
                                     
                                     newline-count
                                     (seq-reduce #'+
                                                 (seq-map (|> (= (string-to-char "\n") %)
                                                              (if % 1 0))
                                                          (when-let* ((start-match (string-match (rx "\n" (0+ (any blank space "\n" )) "\n")
                                                                                                 (buffer-substring-no-properties start end))))
                                                            (substring (buffer-substring-no-properties start end) start-match (match-end 0))))
                                                 0))
                               (pcase (- indent (if (>= 0 newline-count)
                                                  0
                                                  (* (1- newline-count) python-indent-offset)))
                                 ((and (pred natnump) a) a)
                                 (_ 0))))
                            (`(,(or :inside-paren-at-closing-paren
                                    :inside-paren-at-closing-nested-paren) . ,start)
                             (goto-char (+ 1 start))
                             (if (looking-at "[ \t]*\\(?:#\\|$\\)")
                               ;; Copy previous indentation.
                               (current-indentation)
                               ;; Align with opening paren.
                               (current-column)))
                            (`(:inside-docstring . ,start)
                             (let* ((line-indentation (current-indentation))
                                    (base-indent (progn
                                                   (goto-char start)
                                                   (current-indentation))))
                               (max line-indentation base-indent)))
                            (`(,(or :after-block-start
                                    :after-backslash-first-line
                                    :after-backslash-assignment-continuation
                                    :inside-paren-newline-start) . ,start)
                             ;; Add one indentation level.
                             (goto-char start)
                             (+ (current-indentation) python-indent-offset))
                            (`(:after-backslash-block-continuation . ,start)
                             (goto-char start)
                             (let ((column (current-column)))
                               (if (= column (+ (current-indentation) python-indent-offset))
                                 ;; Add one level to avoid same indent as next logical line.
                                 (+ column python-indent-offset)
                                 column)))
                            (`(,(or :inside-paren
                                    :after-backslash-dotted-continuation) . ,start)
                             ;; Use the column given by the context.
                             (goto-char start)
                             (current-column))
                            (`(:after-block-end . ,start)
                             ;; Subtract one indentation level.
                             (goto-char start)
                             (max 0 (- (current-indentation) python-indent-offset)))
                            (`(:at-dedenter-block-start . ,_)
                             ;; List all possible indentation levels from opening blocks.
                             (let ((opening-block-start-points
                                    (python-info-dedenter-opening-block-positions)))
                               (if (not opening-block-start-points)
                                 (prog-first-column) ; if not found default to first column
                                 (mapcar (lambda (pos)
                                           (save-excursion
                                             (goto-char pos)
                                             (current-indentation)))
                                         opening-block-start-points))))
                            (`(,(or :inside-paren-newline-start-from-block) . ,start)
                             (goto-char start)
                             (+ (current-indentation)
                                (* python-indent-offset python-indent-def-block-scale)))
                            (`(,:inside-paren-from-block . ,start)
                             (goto-char start)
                             (let ((column (current-column)))
                               (if (and python-indent-block-paren-deeper
                                        (= column (+ (save-excursion
                                                       (python-nav-beginning-of-statement)
                                                       (current-indentation))
                                                     python-indent-offset)))
                                 (+ column python-indent-offset)
                                 column)))))))
    (indent-line-to (pcase number-or-list
                      ((pred listp)
                       (car number-or-list))
                      (_ number-or-list)))
    (back-to-indentation)))
