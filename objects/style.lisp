;; Parser rules

(make-object 'style
	     :reading-function
	     (lambda (stream)
	       (list 'style (read-to-string stream))))

;; Printers rules

(html-defun 'style
	    (lambda (style)
	      style))

(edit-defun 'style
	    (lambda (style)
	      style))
