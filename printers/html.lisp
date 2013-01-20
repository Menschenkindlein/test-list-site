(def-printer html
    :terminal (lambda (terminal) (princ-to-string terminal))
    :default (lambda (&rest unknown)
	       (apply #'concatenate
		      'string
		      (mapcar #'html
			      unknown))))

(defvar *local-root* nil "relative path to root")

(html-defmacro 'object
	       (lambda (name object-type body)
		 (let ((filename
			(make-pathname
			 :directory (unless (eql 'index object-type)
				      `(:relative ,(name-to-string
						    object-type)))
			 :name (case object-type
				 (index "index")
				 (otherwise name))
			 :type (case object-type
				 (style "css")
				 (otherwise "html")))))
		   (let ((*local-root* (unless (eql 'index object-type)
					 (make-rel-dir :up)))
			 (filename (merge-pathnames
				    filename
				    (merge-pathnames
				     (make-rel-dir "result")
				     *root*))))
		     (unless (probe-file filename)
		       (ensure-directories-exist filename)
		       (with-open-file
			   (file filename
				 :direction :output
				 :if-exists :error
				 :if-does-not-exist :create)
			 (format file (html (case object-type
					      (author (cons object-type
							    (cons name
								  (cdr body))))
					      (otherwise body)))))))
		   (when *local-root*
		     (setf filename
			   (merge-pathnames
			    filename
			    *local-root*)))
		   filename)))
