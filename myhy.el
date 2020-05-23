;;Elisp program for hy source code editing. 
(require 'eros)
(require 'hy-shell)

(defun myhy--eval-last-sexp-string ()
  ( hy-shell--redirect-send   (hy--last-sexp-string)))

(defun myhy-eval-last-sexp ()
  (interactive)
  (mylet [res (myhy--eval-last-sexp-string)]
	 (eros--eval-overlay res (point))))

(setq myhy-result (generate-new-buffer "myhy"))

(defun myhy-eval-last-sexp-buffer ()
  (interactive)
  (mylet [res (myhy--eval-last-sexp-string)]
	 (with-current-buffer myhy-result
	   (erase-buffer)
	   (python-mode)
	   (insert res)
	   (beginning-of-buffer))
	 (switch-to-buffer-other-window myhy-result)))

(provide 'myhy)


