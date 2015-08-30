;;;; portroach.lisp

(in-package #:portroach)

(defparameter *portroach-base-url* "http://portroach.openbsd.org/json/"
  "Base URL all requests are made to.")

(defparameter *cache* (make-hash-table)
  "Cache for retrieved documents. The filename serves as the key.")

(defun make-uri (file)
  "Create the URI to make a request to"
  (concatenate 'string *portroach-base-url* file))

(defun make-request (file &key force)
  "Make a request to Portroach for the JSON file unless it's cached in which
   case it gets served from the cache."
  ;; XXX: should take either a symbol or string.
  ;; can test if it's a string with (typep file :string)
  (flet ((download (file) (add-to-cache file (to-json (flexi-streams:octets-to-string (drakma:http-request (make-uri file)))))))
  	(if force
		(download file)
		(if (cached? file)
	      	(gethash (to-sym file) *cache*)
			(download file)))))

(defun lookup (file)
  "Alias for make-request; either of these should be removed (probably make-request)"
  (make-request file))

(defun summary (&key field)
  "Return the full summary object or just the given :field.
   For example: (summary :field \"total_ports\")"
  (let ((json (jsown:val (lookup "totals.json") "summary")))
    (if field
	(jsown:val json field)
	json)))

(defun print-summary ()
  "Pretty print the summary"
  (let ((json (summary)))
    (jsown:do-json-keys (key value) json
      (format t "~S => ~S~%" key value))))

(defun maintainers ()
  "Get all maintainers including their stats."
  (jsown:val (lookup "totals.json") "results"))

(defun print-maintainers ()
  "Pretty print all maintainers"
  (mapcar 'print-maintainer (maintainers)))

(defun print-maintainer (maintainer)
  "Pretty print maintainer"
  (flet ((maintainer-key (m key) (jsown:val m key)))
    (let* ((data maintainer)
	   (name (maintainer-key data "maintainer"))
	   (total (maintainer-key data "total"))
	   (new (maintainer-key data "withnewdistfile"))
	   (percentage (maintainer-key data "percentage")))
      (format t "~S =>~%  total => ~S~%  new   => ~a~%  percentage => ~f%~&" name total new percentage))))

(defun name-to-uri (name)
  "Transform a given name+email to a proper URI escaping spaces"
  (concatenate 'string (do-urlencode:urlencode name) ".json"))

(defun maintainer-names ()
  "Returns all maintainer names"
  (mapcar (lambda (x) (jsown:val x "maintainer")) (portroach:maintainers)))

(defun contains-maintainer (name line)
  "Return a list with the original line if the name was found in it."
  (when (cl-ppcre:scan name line)
    (list line)))

(defun search-for (name)
  "Search for a given maintainer by name or email in the list of maintainers,
   returning a list of matches."
  (let ((lst (portroach:maintainer-names)))
    (alexandria:flatten
     (delq
      (mapcar (lambda (x) (contains-maintainer name x)) lst)))))

(defun maintainer-to-uri (name &key (direct t) )
  "Lookup the maintainer `name` and return a single uri or `nil`. If
   `direct` is true the shortest match will be used for it's the direct
    match (without co-maintainers), otherwise `ports-for` will fail if
    co-maintainers are found."
  (let ((result (search-for name)))
    (if direct
	(progn
	  (let ((m (car (sort result (lambda (x y) (< (length x) (length y)))))))
	    (if m
		(name-to-uri m)
		(format t "No result found for \"~A\"~%" name))))
	(if (> (length result) 1)
	    (progn
	      (format t "Multiple maintainers found matching: ~A:~%~{\"~A,\"~%~}~%" name result)
	      nil)))))

(defun maintainer-data (name)
  "Lookup all data for `name`"
  (lookup (maintainer-to-uri name)))

(defun ports-for (name)
  "Return list of ports (`basepkgpath`) for a given maintainer"
  (flet ((cons3 (x y z)
	   (alexandria:flatten (cons (cons x y) z))))
    (mapcar (lambda (x) (cons3 (jsown:val x "basepkgpath")
			       (jsown:val x "ver")
			       (jsown:val x "newver"))) (maintainer-data name))))

(defun new-ports-for (name)
  "Return a list of pairs (`basepkgpath` . `newver`) for a given maintainer"
  (delq (mapcar (lambda (x) (unless (null (nth 2 x))
				   (cons
				    (nth 0 x) ; basepkgpath
				    (nth 2 x)))) ; newver
		     (ports-for name))))
