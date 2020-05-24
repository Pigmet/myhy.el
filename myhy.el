;;Elisp program for hy source code editing. 
(require 'eros)
(require 'hy-shell)

;; TODO provide a document lookup utility

(defun myhy--eval-last-sexp-string ()
  (hy-shell--redirect-send (hy--last-sexp-string)))

(defun myhy-eval-last-sexp ()
  (interactive)
  (mylet [res (myhy--eval-last-sexp-string)]
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

;; doc

(setq myhy-doc (generate-new-buffer "*myhy-doc*"))

(defun myhy--doc-as-string (text)
  (hy-shell--redirect-send
   (format "(print %s.__doc__)" text )))

(defun myhy--last-word ()
  (save-excursion
    (re-search-backward (rx (+ word)) (line-beginning-position) t)
    (message "%s"  (thing-at-point 'word))))

(defun myhy-doc ()
  (interactive)
  (mylet [word (myhy--last-word)
	       res (read-string "doc for:" word)
	       s (->>(myhy--doc-as-string res)
		     (s-split "\n")
		     (-drop 1)
		     (s-join "\n"))]
	 (with-current-buffer myhy-doc
	   (erase-buffer)
	   (insert s)
	   (beginning-of-buffer))
	 (switch-to-buffer-other-window myhy-doc)))

(provide 'myhy)
