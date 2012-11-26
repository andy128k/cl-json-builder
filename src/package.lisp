(in-package :cl-user)

(defpackage :json-builder
  (:use :common-lisp
        :common-lisp-user
        :alexandria)
  (:export #:json-string

           #:json-number

           #:json-bool
           #:json-false
           #:json-true

           #:json-null

           #:json-object
           #:json-array

           #:make-json-object
           #:json-object-str))
