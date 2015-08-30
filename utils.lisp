;;;; utils.lisp

(in-package #:portroach)

(defun to-json (data)
  "Convert the given data stream into a JSON object"
  (jsown:parse data))

(defun to-sym (key)
  "Convert a string to a symbol, or return itself when key is already a symbol"
  (if (typep key 'string)
      (intern key)
      key))

(defun to-string (key)
  "Convert a symbol to a string, or return itself when key is already a string"
  (string key))

(defun delq (lst)
  "Remove `nil` elements from `lst`; like `delq` in Emacs Lisp."
  (delete nil lst :test #'equal))
