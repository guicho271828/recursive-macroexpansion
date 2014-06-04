#|
  This file is a part of recursive-macroexpansion project.
  Copyright (c) 2014 guicho
|#

#|
  Author: guicho
|#

(defsystem recursive-macroexpansion
  :version "0.1"
  :author "guicho"
  :mailto ""
  :license ""
  :depends-on (:cl21 :optima)
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "util")
                 (:file "rmacroexpand")
                 (:file "handlers")
                 (:file "hooks"))
                :serial t))
  :description ""
  
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"includes/README.org"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  
  :in-order-to ((test-op (load-op recursive-macroexpansion.test))))
