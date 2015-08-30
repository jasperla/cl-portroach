;;;; portroach.asd

(asdf:defsystem #:portroach
  :description "Query Portroach"
  :author "Jasper Lievisse Adriaanse <j@jasper.la>"
  :version "0.0.1"
  :license "ISC"
  :depends-on (#:drakma
               #:flexi-streams
               #:jsown
	       #:cl-ppcre
	       #:alexandria
	       #:do-urlencode)
  :serial t
  :components ((:file "package")
	       (:file "utils")
	       (:file "cache")
               (:file "portroach")))

