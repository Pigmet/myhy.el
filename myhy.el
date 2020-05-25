;;Elisp program for hy source code editing. 
(require 'eros)
(require 'hy-shell)
(require 'cider-util)
(require 'myhy-shell)


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
;; doc

(setq myhy-doc (generate-new-buffer "*myhy-doc*"))

(defun myhy--doc-as-string (text)
  (hy-shell--redirect-send
   (format
    "(print  %s.__doc__)"
    text text)))

(defun myhy--last-word ()
  (save-excursion
    (re-search-backward
     (rx (+ (not (any blank))))
     (line-beginning-position)
     t)
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


