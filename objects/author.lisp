(make-object 'author
	     :default-structure
	     '(author
	       "Example Author"))

(html-defmacro 'author
	       (lambda (author-nickname author-name)
		 (format nil
			 "~
<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">
<html xmlns=\"http://www.w3.org/1999/xhtml\">
  <head>
    <meta http-equiv=\"content-type\" content=\"text/html; charset=utf-8\" />
    <title>This is the personal page of ~a</title>
  </head>
  <body>
  <h1>~a is great!!!</h1>
  <div class=\"articles-list\">
~{    ~a~}
  </div>
  </body>
</html>"                 author-name
                         author-name
			 (mapcar (lambda (article)
				   (html `(inner-article
					   ,(second article))))
				 (db-select
				  'article
				  (db-where :authors
					    :contains
					    author-nickname))))))



