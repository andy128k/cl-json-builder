;;;; cl-json-builder.asd
;;;;
;;;; Author: Andrey Kutejko <andy128k@gmail.com>

(in-package :asdf)

(defsystem "cl-json-builder"
    :description "JSON building helpers"
    :version "0.1"
    :author "Andrey Kutejko <andy128k@gmail.com>"
    :licence "LLGPL"
    :depends-on (:iterate)
    :serial t
    :components ((:file "src/package")
                 (:file "src/builder")))

