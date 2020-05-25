
(defmacro myhy-with-buffer
    (buffer &rest body)
  "Evaluates body in buffer and switch to it. The content of the buffer gets erased before the evaluation happens."
  `(progn
     (with-current-buffer ,buffer (erase-buffer)  ,@body)
     (switch-to-buffer-other-window ,buffer)))

(defun myhy-shell--valid-hy-form-p(text)
  (not  (s-matches?
	 "\n\nTraceback (most recent call last)"
	 (hy-shell--redirect-send text))))

(setq myhy-comment-regex  (rx bol  "\;;" (+ anything)))

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
		   (s-matches? myhy-comment-regex text)
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
		 valid? (myhy-shell--valid-hy-form-p form)]
	   (append l (list hy-res valid?)))))))

(defun myhy-shell-eval-buffer()
  (interactive)
  (mylet [coll (myhy-shell--eval-buffer-impl)
	       (form start end res _) (-first
				       (-lambda (l)
					 (not (-last-item l)))
				       coll)]
	 (if form
	     (myhy-with-buffer myhy-shell-result
			       (insert (format "Compile error -> %s" form)))
	   (message "buffer was successfully evaluated."))))

;;(myhy-shell--ecal-buffer-impl)





