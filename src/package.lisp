(in-package :cl-user)

(defpackage :json-builder
  (:use :common-lisp
        :common-lisp-user
        :iterate)
  (:export #:json-string

           #:json-number

           #:json-bool
           #:json-false
           #:json-true

           #:json-null

           #:json-object
           #:json-array

           #:make-json-object
           #:json-object-str

           #:json-obj-from-plist
           #:json-obj-from-alist
           #:json-obj-from-hash))

