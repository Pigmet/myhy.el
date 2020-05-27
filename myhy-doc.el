(defun myhy-doc--invalid-form-p (text)
  (s-matches? "\n\nTraceback (most recent call last)"
	      (hy-shell--redirect-send text)))

(defconst myhy-doc-module-items-regex
  (rx "[" (group-n 1 (+ anything)) "]" eol))

(defun myhy-doc--parse-module-items-1 (text)
  (->>
   (hy-shell--redirect-send (format "(dir %s)" text))
   (s-match-strings-all
    myhy-doc-module-items-regex)
   -first-item
   -first-item))

(defun myhy-doc--parse-module-items (text)
  (unless (myhy-doc--invalid-form-p text)
    (->> (myhy-doc--parse-module-items-1 text)
	 (s-split (rx (or "[" "," "'" "]")))
	 (-map 's-trim)
	 (-remove (-lambda (s) (zerop (length s)))))))

(defun myhy-doc--doc-string (text)
  "Returns the string of document of text."
  (hy-shell--redirect-send
   (format "(print  %s.__doc__)" text)))

(defun myhy-doc-insert-module-item()
  "Displays the constants and functions defined in the module
specified by the string preceding the current point."
  (interactive)
  (mylet [text (myhy--last-word)
	       res (ido-completing-read
		    "select: "
		    (myhy-doc--parse-module-items text))]
	 (re-search-backward
	  (rx (+ (not (any blank))))
	  (line-beginning-position)
	  t)
	 (forward-char 1)
	 (insert "." res)))

;; TODO add doc lookup

(defun myhy-doc--build-doc (text)
  (mylet [sig (myhy--signature-string text)
	      doc (myhy--doc-as-string text)]
	 (with-temp-buffer
	   (insert text "\n\n")
	   (when sig (insert sig "\n\n"))
	   (insert doc)
	   (buffer-string))))

(setq myhy-doc-buffer
      (generate-new-buffer "myhy-doc"))

(setq myhy-doc-item-doc-buffer
      (generate-new-buffer "myhy-doc-item"))

(defun myhy-doc-view-module-methods()
  (interactive)
  (mylet [module (myhy--last-word)
		 coll (myhy-doc--parse-module-items module)]
	 (myhy-with-buffer
	  myhy-doc-buffer
	  (insert (format "in %s:" module) "\n\n")
	  (loop for s in coll
		do
		(insert-text-button
		 s
		 'action
		 (lexical-let ((s s) (module module))
		   (-lambda (b)
		     (myhy-with-buffer
		      myhy-doc-item-doc-buffer
		      (insert
		       (myhy-doc--build-doc
			(concat module "." s)))
		      (beginning-of-buffer)))))
		(insert  "\n"))
	  (beginning-of-buffer))))

(provide 'myhy-doc)


