;; Parser rules

(default-copy-grammar article)

(make-object 'article
	     :default-structure
	     '(article
	       (title "Example title")
	       (authors-list "exmpl")
               (date "11" "11" "2111")
	       (text
		(paragraph
		 "Example paragraph.")))
	     :reading-function
             (lambda (stream)
               (article-parse
                'article
                (read-to-string stream)))
             :helper-maker
             (lambda (&rest body)
               (destructuring-bind (ar
                                    (tit title)
                                    (al . authors-list)
                                    (dt . date)
                                    text) body
                 (declare (ignore ar tit al dt text))
                 `(:title ,title
                   :authors ,authors-list
                   :date ,(apply #'convert-date
                                 (mapcar #'read-from-string date))))))

(article-defrule article (and title authors date text)
  (:lambda (result)
    (cons 'article result)))

(article-defrule title paragraph
  (:lambda (result)
    (cons 'title (cdr result))))

(article-defrule authors paragraph
  (:lambda (result)
    (cons 'authors (article-parse '(+ word) (second result)))))

(article-defrule date paragraph
  (:lambda (result)
    (cons 'date (article-parse '(+ word) (second result)))))

(article-defrule text (* paragraph)
  (:lambda (result)
    (cons 'text result)))

(article-defrule paragraph-text (+ (or emphasis
				       inner-article
                                       inner-author
				       character)))

(article-defrule emphasis (and "em{" (+ (and (! #\}) character)) "}")
  (:destructure (em text me)
    (declare (ignore em me))
    (list 'emphasis (text text))))

(article-defrule inner-article (and "article{"
				    (+ (and (! #\}) character))
				    "}")
  (:destructure (ar text ra)
    (declare (ignore ar ra))
    (list 'inner-article (text text))))

(article-defrule inner-author (and "author{"
				    (+ (and (! #\}) character))
				    "}")
  (:destructure (au text ua)
    (declare (ignore au ua))
    (list 'inner-author (text text))))

;; Printers rules

(html-defmacro 'article
	       (lambda (title authors date text)
		 (format nil
			 "~
<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">
<html xmlns=\"http://www.w3.org/1999/xhtml\">
  <head>
    <meta http-equiv=\"content-type\" content=\"text/html; charset=utf-8\" />
    <title>~a</title>
  </head>
  <body>
  ~a
  ~a
  ~a
  ~a
  </body>
</html>"       (second title)
               (html title)
	       (html authors)
	       (html date)
	       (html text))))

(html-defun 'title
	    (lambda (title)
	      (format nil "  <h1>~a</h1>" title)))

(html-defun 'date
	    (lambda (day month year)
	      (format nil "  <p>~a ~a ~a</p>" day month year)))

(html-defmacro 'authors
	       (lambda (&rest authors)
		 (format nil "  <p class=\"authors\">~{~a~^ ~}</p>"
			 (mapcar (lambda (author)
				   (html (list 'inner-author author)))
				 authors))))

(html-defmacro 'inner-author
	       (lambda (author)
		 (let ((author (db-get 'author
				       author)))
		   (format nil "<a href=\"~a\">~a</a>"
			   (html author)
			   (second (fourth author))))))

(html-defmacro 'inner-article
	       (lambda (article)
		 (let ((article (db-get 'article
					article)))
		   (format nil "<a href=\"~a\">~a</a>"
			   (html article)
			   (second (second (fourth article)))))))

(html-defun 'text
	    (lambda (&rest paragraphs)
	      (format nil "  <div>~%~{      ~a~%~}    </div>"
		      paragraphs)))

(html-defmacro 'paragraph
	       (lambda (paragraph)
		 (format nil "<p>~{~a~}</p>"
			 (mapcar #'html
				 (article-parse 'paragraph-text
						paragraph)))))

(html-defun 'emphasis
	    (lambda (emphasized)
	      (format nil "<i>~a</i>" emphasized)))

(edit-defmacro 'article
	       (lambda (title authors date text)
		 (format nil
			 "~
~a

~a

~a

~a
"                        (second title)
			 (edit authors)
			 (edit date)
			 (edit text))))

(edit-defun 'date
	    (lambda (day month year)
	      (format nil "~a ~a ~a" day month year)))

(edit-defun 'authors
	    (lambda (&rest authors)
	      (format nil "~{~a~^ ~}"
		      authors)))

(edit-defun 'text
	    (lambda (&rest text)
	      (format nil "~{~a~^~%~%~}"
		      text)))

(edit-defmacro 'paragraph
	       (lambda (paragraph)
		 (format nil "~{~a~}"
			 (mapcar (lambda (item)
				   (if (characterp item)
				       item
				       (edit item)))
				 (article-parse 'paragraph-text
						paragraph)))))

(edit-defun 'emphasis
	    (lambda (emphasized)
	      (format nil "em{~a}" emphasized)))

(edit-defun 'inner-article
	    (lambda (article)
	      (format nil "article{~a}"
		      article)))

(edit-defun 'inner-author
	    (lambda (author)
	      (format nil "author{~a}"
		      author)))