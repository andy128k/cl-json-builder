(in-package :json-builder)

(defstruct json-object str)

(defun %json-string (val)
  (with-output-to-string (stream)
    (write-string "\"" stream)
    (loop
       for char across val
       do (case char
            (#\"         (write-string "\\\"" stream))
            (#\\         (write-string "\\\\" stream))
            (#\/         (write-string "\\/" stream))
            (#\Backspace (write-string "\\b" stream))
            (#\Tab       (write-string "\\t" stream))
            (#\Newline   (write-string "\\n" stream))
            (#\Page      (write-string "\\f" stream))
            (#\Return    (write-string "\\r" stream))
            (t (cond
                 ((<= 0 (char-code char) #x1f)
                  (format stream "\\u~4,'0x" (char-code char)))
                 (t (write-char char stream))))))
    (write-string "\"" stream)))

(defun %json-number (val)
  (format nil "~A" val))

(defun %json-bool (val)
  (if val "true" "false"))

(defgeneric json-simple-value (val)
  (:method ((val json-object))
    (json-object-str val))
  (:method ((val string))
    (%json-string val))
  (:method ((val number))
    (%json-number val))
  (:method (val)
    (%json-bool val)))

(defun json-string (val)
  (make-json-object :str (%json-string val)))

(defun json-number (val)
  (make-json-object :str (%json-number val)))

(defun json-bool (val)
  (make-json-object :str (%json-bool val)))

(defun json-false ()
  (make-json-object :str "false"))

(defun json-true ()
  (make-json-object :str "true"))

(defun json-null ()
  (make-json-object :str "null"))

(defun json-array (&rest values)
  (make-json-object :str (with-output-to-string (stream)
                           (write-char #\[ stream)
                           (loop
                              for v in values
                              for c = nil then t
                              do
                                (when c
                                  (write-char #\, stream))
                                (write-string (json-simple-value v) stream))
                           (write-char #\] stream))))

(defmacro define-object-loops (name (&rest params) &body body)
  `(progn
     (defun ,(intern (format nil "~A-PLIST" name)) ,params
       (macrolet ((object (o k v)
                    `(for (,k ,v) on ,o by #'cddr)))
         ,@body))
     (defun ,(intern (format nil "~A-ALIST" name)) ,params
       (macrolet ((object (o k v)
                    `(for (,k . ,v) in ,o)))
         ,@body))
     (defun ,(intern (format nil "~A-HASH" name)) ,params
       (macrolet ((object (o k v)
                    `(for (,k ,v) in-hashtable ,o)))
         ,@body))))

(defun %key-name (k)
  (if (symbolp k)
      (with-output-to-string (stream)
	(loop
	   for ch across (symbol-name k)
	   do (if (char= ch #\-)
		  (write-char #\_ stream)
		  (write-char (char-downcase ch) stream))))
      (string k)))

(define-object-loops json-obj-from (obj)
  (make-json-object :str (with-output-to-string (stream)
                           (write-char #\{ stream)
                           (iter (object obj k v)
                                 (for c initially nil then t)
                                 (when c
                                   (write-char #\, stream))
                                 (write-string (%json-string (%key-name k)) stream)
                                 (write-char #\: stream)
                                 (write-string (json-simple-value v) stream))
                           (write-char #\} stream))))

(defun json-object (&rest keys-and-values)
  (make-json-object :str (with-output-to-string (stream)
                           (write-char #\{ stream)
                           (loop
                              for (k v) on keys-and-values by #'cddr
                              for c = nil then t
                              do
                                (when c
                                  (write-char #\, stream))
                                (write-string (%json-string (%key-name k)) stream)
                                (write-char #\: stream)
                                (write-string (json-simple-value v) stream))
                           (write-char #\} stream))))

