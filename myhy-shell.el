(defmacro myhy-with-buffer
    (buffer &rest body)
  "Evaluates body in buffer and switch to it. The content of the buffer gets erased before the evaluation happens."
  `(progn
     (with-current-buffer ,buffer (erase-buffer)  ,@body)
     (switch-to-buffer-other-window ,buffer)))

(defconst myhy-shell-comment-regex  (rx bol "\;;" (+ anything)))

(defconst myhy-shell-compile-error-regex "\n\nTraceback (most recent call last)")

(defun myhy-shell--get-all-position-sexp-buffer()
  (mylet [res (list)
	      start 0
	      end 1]
	 (save-excursion
	   (goto-char (point-min))
	   (while (< start end)
	     (setq start (point))
	     (cider-start-of-next-sexp 1 )
	     (setq end (point))
	     (push (list start end) res)))
	 (reverse res)))

;; com

(defun myhy-shell--get-all-sxep-buffer ()
  "Returns list of (sexp start end) in this buffer, where start and end
are the location of the sexp."
  (->> (myhy-shell--get-all-position-sexp-buffer)
       (-map (-lambda ((start end)) (list
				     (s-trim
				      (buffer-substring-no-properties start end))
				     start
				     end)))
       (-remove (-lambda ((text _ _))
		  (or
		   (s-matches? myhy-shell-comment-regex text)
		   (zerop (length text)))))))

(setq myhy-shell-result (generate-new-buffer "myhy-shell"))

(defun myhy-shell--eval-buffer-impl ()
  "Returns list of (form start end res valid?)"
  (->> (myhy-shell--get-all-sxep-buffer)
       (-map
	(-lambda (l)
	  (mylet
	   [form (-first-item l)
		 hy-res (hy-shell--redirect-send form)
		 valid? (not (s-matches? myhy-shell-compile-error-regex  hy-res))]
	   (append l (list hy-res valid?)))))))

;; TODO: be more kind to the user and imporve the debugging information.

(defun myhy-shell--goto-error-button (label f pos)
  (insert-text-button label
		      'action
		      (lexical-let ((f f) (pos pos))
			(find-file-other-window f)
			(goto-char pos ))))

(defun myhy-shell-eval-buffer()
  (interactive)
  (mylet [f (buffer-file-name)
	    coll (myhy-shell--eval-buffer-impl)
	    (form start end res _) (-first
				    (-lambda (l)
				      (not (-last-item l)))
				    coll)]
	 (if form ;; if the result is of compile error.
	     (myhy-with-buffer
	      myhy-shell-result
	      (insert-text-button f
				  'action
				  (lexical-let ((f f)(start start))
				    (-lambda (b)
				      (find-file-other-window f)
				      (goto-char start))))
	      (insert "\n\n" res)
	      (python-mode)
	      (beginning-of-buffer))
	   (message "success"))))

(provide 'myhy-shell)
