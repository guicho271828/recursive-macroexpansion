
(in-package :cl21-user)
(in-package :recursive-macroexpansion)

(define-condition compile-time-condition (simple-condition) ())
(define-condition compile-definition (compile-time-condition)
  ((name :initarg :name :accessor name))
  (:documentation "a condition signalled when it compiles a new definition"))
(define-condition compile-redefinition (compile-definition) ()
  (:documentation "a condition signalled when it compiles a redefinition of already defined concept"))

(defmacro define-definition-conditions (name-head other-classes
                                        slots doc &optional
                                                    (doc-definition doc)
                                                    (doc-redefinition doc-definition))
  (let ((c (alexandria:symbolicate "COMPILE-" name-head))
        (d (alexandria:symbolicate name-head "-DEFINITION"))
        (r (alexandria:symbolicate name-head "-REDEFINITION")))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (define-condition ,c (compile-time-condition ,@other-classes) ,slots
         (:documentation ,doc))
       (define-condition ,d (,c compile-definition) ()
         (:documentation ,doc-definition))
       (define-condition ,r (,c compile-redefinition) ()
         (:documentation ,doc-redefinition)))))

(define-condition args-mixin ()
  ((args :initarg :args :accessor args)))
(define-condition body-mixin ()
  ((body :initarg :body :accessor body)))

(define-definition-conditions function (args-mixin body-mixin) ()
  "a condition signalled when compiling defun, defgeneric, defmacro, define-compiler-macro, defmethod")
(define-definition-conditions variable (body-mixin) ()
    "a condition signalled when compiling defvar,defparameter")
(define-definition-conditions class ()
  ((superclasses :initarg :superclasses :accessor superclasses)
   (options :initarg :options :accessor options)
   (slots :initarg :slots :accessor slots))
    "a condition signalled when compiling defclass")
(define-definition-conditions condition (compile-class) ()
    "a condition signalled when compiling defclass")

(macrolet ((expand-fun (target)
             `(defexpand ,target (&whole whole name args &body body &environment env)
                (signals (if (fboundp name) 'function-redefinition 'function-definition)
                         :name name :args args :body body)
                (macroexpand whole env))))
  (expand-fun defun)
  (expand-fun defgeneric)
  (expand-fun defmacro)
  (expand-fun define-compiler-macro)
  (expand-fun defmethod))

(macrolet ((expand-var (target)
             `(defexpand ,target (&whole whole name &body body &environment env)
                (signals (if (boundp name) 'variable-redefinition 'variable-definition)
                         :name name
                         :body body)
                (macroexpand whole env))))
  (expand-fun defvar)
  (expand-fun defparameter))

(macrolet ((expand-class (target def redef)
             `(defexpand ,target (&whole whole name superclasses slots &rest options &environment env)
                (signals (if (find-class name nil env) ',def ',redef)
                         :name name
                         :superclasses superclasses
                         :slots slots
                         :options options)
                (macroexpand whole env))))
  (expand-fun defclass class-definition class-redefinition)
  (expand-fun define-condition condition-definition condition-redefinition))

