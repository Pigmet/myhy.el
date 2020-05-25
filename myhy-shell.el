;; evaluate the entire buffer. 

(defmacro myhy-with-buffer
    (buffer &rest body)
  "Evaluates body in buffer and switch to it. The content of the buffer gets erased before the evaluation happens."
  `(progn
     (with-current-buffer ,buffer (erase-buffer)  ,@body)
     (switch-to-buffer-other-window ,buffer)))

;; regex for error message 

(defconst myhy-shell-compile-error-regex "\n\nTraceback (most recent call last)")

(defun myhy-shell--get-all-position-sexp-buffer()
  (mylet [res (list)
	      start 0
	      end 1]
	 (save-excursion
	   (goto-char (point-min))
	   (while (< start end)
	     (setq start (point))
	     (cider-start-of-next-sexp 1)
	     (setq end (point))
	     (push (list start end) res)))
	 (reverse res)))

(defconst myhy-shell-comment-regex
  (rx bol  "\;;" (+ anything)  ))

;; com

;; TODO : remove comment from the parse result

(defun myhy-shell--remove-comment (s)
  (->> (s-split "\n" s)
       (-remove (-lambda (x) (s-matches? (rx bol "\;;") x)))
       (s-join "\n")))
					;
(defun myhy-shell--get-all-sexp-buffer-string ()
  (->> (myhy-shell--get-all-position-sexp-buffer)
       (-map (-lambda ((start end))
	       (list
		(s-trim
		 (buffer-substring-no-properties start end))
		start
		end)))))

(defun myhy-shell--get-all-sexp-buffer ()
  (->> (myhy-shell--get-all-sexp-buffer-string)
       (-map (-lambda ((text start end))
	       (list
		(myhy-shell--remove-comment text)
		start
		end)))))

(setq myhy-shell-result (generate-new-buffer "myhy-shell"))

(defun myhy-shell-view-all-sexp ()
  (interactive)
  (mylet [s (->> (myhy-shell--get-all-sexp-buffer)
		 (-map '-first-item)
		 (s-join "\n\n"))]
	 (myhy-with-buffer myhy-shell-result
			   (save-excursion
			     (hy-mode)
			     (insert s)))))

(defun myhy-shell-view-all-comments ()
  (interactive))

(defun myhy-shell--eval-buffer-impl ()
  "Returns list of (form start end res valid?)"
  (->> (myhy-shell--get-all-sexp-buffer)
       (-map
	(-lambda (l)
	  (mylet
	   [form (-first-item l)
		 hy-res (hy-shell--redirect-send form)
		 valid? (not (s-matches? myhy-shell-compile-error-regex  hy-res))]
	   (append l (list hy-res valid?)))))))

;; TODO: be more kind to the user and imporve the debugging information.

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

