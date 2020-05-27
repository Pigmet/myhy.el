;; evaluate the entire buffer. 

(defmacro myhy-with-buffer
    (buffer &rest body)
  "Evaluates body in buffer and switch to it. The content of the buffer gets erased before the evaluation happens."
  `(progn
     (with-current-buffer ,buffer (erase-buffer)  ,@body)
     (switch-to-buffer-other-window ,buffer)))

;; regex for error message 

(defconst myhy-shell-compile-error-regex
  "\n\nTraceback (most recent call last)")

;; is this ok?
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

;; remove comment from the parse result

(defun myhy-shell--remove-comment (s)
  (->> (s-split "\n" s)
       (-remove (-lambda (x) (s-matches? (rx bol "\;;") x)))
       (s-join "\n")))
					;
(defun myhy-shell--get-all-sexp-buffer-string ()
  "Returns list of (form start end)"
  (->> (myhy-shell--get-all-position-sexp-buffer)
       (-map (-lambda ((start end))
	       (list
		(s-trim
		 (buffer-substring-no-properties start end))
		start
		end)))))

(defun myhy-shell--get-all-sexp-buffer ()
  "Returns list of (form start end)"
  (->> (myhy-shell--get-all-sexp-buffer-string)
       (-map (-lambda ((text start end))
	       (list
		(myhy-shell--remove-comment text)
		start
		end)))
       (-map (-lambda ((form start end))
	       (list (s-trim form) start end)))))

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

(defun myhy-shell--valid-form? (res)
  (->> res
       (s-split "\n\n")
       -last-item
       (s-matches? "Traceback (most recent call last)")
       not))

(defun myhy-shell--invalid-form? (res)
  (not (myhy-shell--valid-form? res)))

(defun myhy-shell--eval-buffer-impl ()
  "Returns list of (form start end res valid?)"
  (->> (myhy-shell--get-all-sexp-buffer)
       (-map
	(-lambda (l)
	  (mylet
	   [form (-first-item l)
		 hy-res (hy-shell--redirect-send form)
		 valid? (myhy-shell--valid-form? hy-res)]
	   (append l (list hy-res valid?)))))))

;; TODO: be more kind to the user and imporve the debugging information.

;; test method. the hy repl seems insecure.
(defun myhy-shell-eval-buffer-display()
  (interactive)
  (mylet [forms (->> (myhy-shell--get-all-sexp-buffer)
		     (-map '-first-item))]
	 (myhy-with-buffer myhy-shell-result
			   (erase-buffer)
			   (loop for form in forms
				 do
				 (insert
				  form
				  "\n"
				  (hy-shell--redirect-send form)
				  "\n")))))

(defun myhy-shell--get-first-error ()
  (->> (myhy-shell--get-all-sexp-buffer)
       (-map (-lambda ((f start end))
	       (list (hy-shell--redirect-send f) start end)))
       (-first (-lambda ((f start end)) (myhy-shell--invalid-form? f)))))

(defun myhy-shell-eval-buffer ()
  (interactive)
  (mylet [fname (buffer-file-name)
		(form start end) (myhy-shell--get-first-error)]
	 (if form
	     (myhy-with-buffer
	      myhy-shell-result
	      (save-excursion
		(python-mode)
		(insert-text-button
		 fname
		 'action
		 (lexical-let
		     ((start start) (fname fname))
		   (-lambda (b)
		     (find-file fname)
		     (goto-char start))))
		(insert "\n\n" form)))
	   (message "success"))))

;; TODO: add eval-region

;;(myhy-shell--invalid-form?)

(provide 'myhy-shell)

