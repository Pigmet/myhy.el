;;Elisp program for hy source code editing. 

(defun myhy-eval-last-sexp ()
  (interactive)
  (mylet [res (myhy--send-text (hy--last-sexp-string))]
	 (eros--eval-overlay res (point))))

(provide 'myhy)


