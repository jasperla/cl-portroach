;;;; cache.lisp

(in-package #:portroach)

(defun cached? (file)
  "Check if the filename has been cached."
  (if (gethash (to-sym file) *cache*)
      t
      nil))

(defun add-to-cache (filename content)
  "Add the filename to the cache, if the filename already exists it will be updated"
  (setf (gethash (to-sym filename) *cache*) content))

(defun print-cache-files (key value)
    (declare (ignore value))
    (format t "~S~%" key))

(defun print-cache-entries (key value)
    (format t "~S => ~S~%" key value))

(defun print-cache (&key (only-filenames t))
  "Print the full cache, or only the filenames if :only-filenames is set."
  (if only-filenames
      (maphash #'print-cache-files *cache*)
      (maphash #'print-cache-entries *cache*)))

(defun clean-cache ()
  "Purge the entire cache."
  (clrhash *cache*))
