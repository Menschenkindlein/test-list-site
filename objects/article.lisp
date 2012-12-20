;; Parser rules

(default-copy-grammar article)

(make-object 'article
	     :default-structure
	     '(article
	       (title "Example title")
	       (authors-list "exmpl")
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
                                    text) body
                 (declare (ignore ar tit al text))
                 `(:title ,title
                          :authors ,authors-list))))

(article-defrule article (and title authors text)
  (:lambda (result)
    (cons 'article result)))

(article-defrule title paragraph
  (:lambda (result)
    (cons 'title (cdr result))))

(article-defrule authors paragraph
  (:lambda (result)
    (cons 'authors (article-parse 'authors-list (second result)))))

(article-defrule authors-list (+ word))

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
			      (mapcar (lambda (author)
                                        (html (list 'inner-author author)))
                                      authors))))

(add-html-structure 'inner-author
		    (lambda (author)
                      (let ((author (db-get 'author
					    author)))
                        (format nil "<a href=\"~a\">~a</a>"
                                (html author)
                                (second (fourth author))))))

(add-html-structure 'inner-article
		    (lambda (article)
                      (let ((article (db-get 'article
                                             article)))
		      (format nil "<a href=\"~a\">~a</a>"
                              (html article)
			      (second (second (fourth article)))))))

(add-html-structure 'text
		    (lambda (&rest paragraphs)
		      (format nil "  <div>~%~{      ~a~%~}    </div>"
			      (mapcar #'html
				      paragraphs))))

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
			      authors)))

(add-edit-structure 'text
		    (lambda (&rest text)
		      (format nil "~{~a~^~%~%~}"
			      (mapcar #'edit
				      text))))

(add-edit-structure 'paragraph
		    (lambda (paragraph)
		      (format nil "~{~a~}"
			      (mapcar (lambda (item)
					(if (characterp item)
					    item
					    (edit item)))
				      (article-parse 'paragraph-text
						     paragraph)))))

(add-edit-structure 'emphasis
		    (lambda (emphasized)
		      (format nil "em{~a}" emphasized)))

(add-edit-structure 'inner-article
		    (lambda (article)
		      (format nil "article{~a}"
			      article)))

(add-edit-structure 'inner-author
		    (lambda (author)
		      (format nil "author{~a}"
			      author)))