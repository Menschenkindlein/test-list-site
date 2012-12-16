;; Parser rules

(make-object 'style
	     :reading-function
	     (lambda (stream)
	       (list 'style (read-to-string stream))))

;; Printers rules

(add-html-structure 'style
		    (lambda (style)
		      style))

(add-edit-structure 'style
		    (lambda (style)
		      style))
