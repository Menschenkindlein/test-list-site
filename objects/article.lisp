;; Parser rules

(default-copy-grammar article)

(make-object 'article
	     :default-structure
	     '(article
	       (title "Example title")
	       (authors-list
		(author "exmpl"))
	       (text
		(paragraph
		 "Example paragraph.")))
	     :reading-function (lambda (stream)
				 (article-parse
				  'article
				  (read-to-string stream))))

(article-defrule article (and title authors raw-text)
  (:lambda (result)
    (cons 'article result)))

(article-defrule title paragraph
  (:lambda (result)
    (cons 'title (cdr result))))

(article-defrule authors paragraph
  (:lambda (result)
    (cons 'authors (article-parse 'authors-list (second result)))))

(article-defrule authors-list (+ inner-author))

(article-defrule inner-author (and (+ (and (! #\Space) character))
				   (? #\Space))
  (:destructure (author rest)
    (declare (ignore rest))
    (list 'inner-author (text author) (text author))))

(article-defrule raw-text (* character)
  (:lambda (result)
    (list 'text (text result))))

(article-defrule text (* paragraph))

(article-defrule paragraph-text (+ (or emphasis
				       inner-article
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
    (list 'inner-article (text text) (text text))))

;; Printers rules

(add-html-structure 'article
		    (lambda (title authors text)
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
  </body>
</html>"                      (second title)
                              (html title)
			      (html authors)
			      (html text))))

(add-html-structure 'title
		    (lambda (title)
		      (format nil "  <h1>~a</h1>" title)))

(add-html-structure 'authors
		    (lambda (&rest authors)
		      (format nil "  <p class=\"authors\">~{~a~^ ~}</p>"
			      (mapcar #'html authors))))

(add-html-structure 'inner-author
		    (lambda (link-text author)
		      (format nil "<a href=\"~a\">~a</a>"
			      (html (db-get 'author
					    author))
			      link-text)))

(add-html-structure 'inner-article
		    (lambda (link-text article)
		      (format nil "<a href=\"~a\">~a</a>"
			      (html (db-get 'article
					    article))
			      link-text)))

(add-html-structure 'text
		    (lambda (text)
		      (format nil "  <div>~%~{      ~a~%~}    </div>"
			      (mapcar #'html
				      (article-parse 'text text)))))

(add-html-structure 'paragraph
		    (lambda (paragraph)
		      (format nil "<p>~{~a~}</p>"
			      (mapcar #'html
				      (article-parse 'paragraph-text
						     paragraph)))))

(add-html-structure 'emphasis
		    (lambda (emphasized)
		      (format nil "<i>~a</i>" emphasized)))

(add-edit-structure 'article
		    (lambda (title authors text)
		      (format nil
			      "~
~a

~a

~a
"                             (second title)
			      (edit authors)
			      (edit text))))

(add-edit-structure 'authors
		    (lambda (&rest authors)
		      (format nil "~{~a~^ ~}"
			      (mapcar #'edit authors))))

(add-edit-structure 'text
		    (lambda (text)
		      (format nil "~{~a~^~%~%~}"
			      (mapcar #'edit
				      (article-parse 'text text)))))

(add-edit-structure 'paragraph
		    (lambda (paragraph)
		      (format nil "~{~a~}"
			      (mapcar (lambda (item)
					(if (eq (type-of item)
						'standard-char)
					    item
					    (edit item)))
				      (article-parse 'paragraph-text
						     paragraph)))))

(add-edit-structure 'emphasis
		    (lambda (emphasized)
		      (format nil "em{~a}" emphasized)))

(add-edit-structure 'inner-article
		    (lambda (link-text article)
		      (declare (ignore link-text))
		      (format nil "article{~a}"
			      article)))

(add-edit-structure 'inner-author
		    (lambda (link-text author)
		      (declare (ignore link-text))
		      (format nil "~a"
			      author)))