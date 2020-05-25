;;Elisp program for hy source code editing. 
(require 'eros)
(require 'hy-shell)
(require 'cider-util)


;; TODO 1 provide a document lookup utility
;; 2 signal compilation error 

(defun myhy--eval-last-sexp-string ()
  (hy-shell--redirect-send (hy--last-sexp-string)))

(defun myhy--eval-last-sexp-string-only-result ()
  (->> (myhy--eval-last-sexp-string)
       (s-split "\n\n")
       -last-item))

;; TODO: get rid of the string representation of objetcs to display
(defun myhy-eval-last-sexp ()
  (interactive)
  (mylet [res (myhy--eval-last-sexp-string-only-result)]
	 (eros--eval-overlay res (point))))

(setq myhy-result (generate-new-buffer "*myhy-result*"))

(defun myhy-eval-last-sexp-buffer ()
  (interactive)
  (mylet [res (myhy--eval-last-sexp-string)]
	 (with-current-buffer myhy-result
	   (erase-buffer)
	   (python-mode)
	   (insert res)
	   (beginning-of-buffer))
	 (switch-to-buffer-other-window myhy-result)))

;; eval whole buffer (hy-mode seems to buggy)

;;First parse all the s-expressions in the buffer,
;; then let hy evaluate each of them.

(defun myhy--all-sexp-buffer-1()
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
	 (->> res
	      reverse
	      (-map (-lambda ((start end))
		      (buffer-substring-no-properties start end)))
	      (-map 's-trim))))

;; TODO : add hide-comment function 
(defun myhy--all-sexp-buffer (&optional remove-comment?)
  (mylet [res (myhy--all-sexp-buffer-1)]
	 (if remove-comment?
	     (-remove (-lambda (s) (s-matches-p
				    (rx bol  "\;;" (+ anything))
				    s))
		      res)
	   res)))

(defun myhy-view-all-sexp-buffer()
  (interactive)
  (mylet [forms (myhy--all-sexp-buffer t)]
	 (with-current-buffer myhy-result
	   (erase-buffer)
	   (hy-mode)
	   (insert (s-join "\n\n" forms))
	   (beginning-of-buffer))
	 (switch-to-buffer-other-window myhy-result)))

(defun myhy--eval-buffer-list()
  (interactive)
  (setq res (list))
  (loop for s in (myhy--all-sexp-buffer)
	do
	(push (hy-shell--redirect-send s) res))
  (reverse res))

(defun myhy-eval-buffer ()
  (interactive)
  (mylet [forms (myhy--eval-buffer-list)]
	 (with-current-buffer myhy-result
	   (erase-buffer)
	   (save-excursion
	     (python-mode)
	     (insert (s-join "\n" forms))))
	 (switch-to-buffer-other-window myhy-result)))

;; doc

(setq myhy-doc (generate-new-buffer "*myhy-doc*"))

(defun myhy--doc-as-string (text)
  (hy-shell--redirect-send
   (format "(print %s.__doc__)" text )))

(defun myhy--last-word ()
  (save-excursion
    (re-search-backward (rx (+ (not (any blank)))) (line-beginning-position) t)
    (if-let ((res (thing-at-point 'symbol)))
	(message "%s" res)
      "")))

(defun myhy-doc ()
  (interactive)
  (mylet [word (myhy--last-word)
	       res (read-string "doc for:" word)
	       s (myhy--doc-as-string res)]
	 (with-current-buffer myhy-doc
	   (erase-buffer)
	   (python-mode)
	   (insert s)
	   (beginning-of-buffer))
	 (switch-to-buffer-other-window myhy-doc)))

(provide 'myhy)


