#|
  This file is a part of recursive-macroexpansion project.
  Copyright (c) 2014 guicho
|#


(in-package :cl-user)
(defpackage recursive-macroexpansion.test-asd
  (:use :cl :asdf))
(in-package :recursive-macroexpansion.test-asd)


(defsystem recursive-macroexpansion.test
  :author "guicho"
  :license ""
  :depends-on (:recursive-macroexpansion
               :fiveam)
  :components ((:module "t"
                :components
                ((:file "package"))))
  :perform (load-op :after (op c) (PROGN
 (EVAL (READ-FROM-STRING "(fiveam:run! :recursive-macroexpansion)"))
 (CLEAR-SYSTEM C))))
