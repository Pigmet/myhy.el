
(hy-shell--redirect-send "(import [numpy :as num])")

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

;; use this?
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

(setq myhy-doc-buffer (generate-new-buffer "myhy-doc"))

;; unfinished
(defun myhy-doc-list-module-items ()
  (interactive)
  (mylet [text (myhy--last-word)
	       coll (myhy-doc--parse-module-items text)]
	 (myhy-with-buffer myhy-doc-buffer)))



(provide 'myhy-doc)


