#|
This file is a part of recursive-macroexpansion project.
Copyright (c) 2014 guicho
|#

(in-package :cl21-user)
(defpackage :recursive-macroexpansion
  (:use :cl21 :cl21.core :cl21.core.environment
        :optima)
  (:export :defexpand
           :rmacroexpand
           :recursive-macro-function
           :with-recursive-macro-expansion))
