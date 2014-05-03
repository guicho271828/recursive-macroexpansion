#|
  This file is a part of recursive-macroexpansion project.
  Copyright (c) 2014 guicho
|#

(in-package :cl21-user)
(defpackage :recursive-macroexpansion.test
  (:use :cl21
        :recursive-macroexpansion
        :fiveam))
(in-package :recursive-macroexpansion.test)

(def-suite :recursive-macroexpansion)
(in-suite :recursive-macroexpansion)

;; run test with (run! test-name) 
;;   test as you like ...


;; * Examples
;;
;; Ordinary macro expands one layer only.  Further expansions are implicitly done by
;; the other call to macroexpand-1, which users cannot control.
;; Therefore, we are not able to trap the conditions signalled
;; in the expansion of the subforms.

;; For example, the combination below is a rather stupid example:
(defun stupid-error-handler (c)
  (format *standard-output* "~&ignored a stupid error")
  (continue c))

(defmacro with-let (var-form value-form &body body)
  (handler-bind ((error #'stupid-error-handler))
    `(let ((,var-form ,value-form))
       ,@body)))

(defmacro limited-progn (&body body)
  ;; stupid assertion
  (unless (< (length body) 3)
    (cerror "Ignore!" "the body of limited-progn should be of length less than 3 !"))
  `(progn ,@body))

(test macroexpansion
  (is (tree-equal
       '(let ((x 1)) (print x))
       (macroexpand '(with-let x 1 (print x)))))

  ;; expansion works normally if nothing is signalled
  (finishes
    (with-let x 1 (limited-progn 1 2)))

  ;; the error is not trapped, so it is visible from the outside
  (signals error
    (with-let x 1 (limited-progn 1 2 3 4))))


;; On the other hand, in `defexpand', we can control the further expansion.
;; What's special here is `rmacroexpand'.

(defexpand with-let (var-form value-form &body body)
  (handler-bind ((error #'stupid-error-handler))
    (rmacroexpand
     `(let ((,var-form ,value-form))
        ,@body))))

(test recursive-macroexpansion
  (is (tree-equal
       '(let ((x 1)) (print x))
       (rmacroexpand '(with-let x 1 (print x)))))

  ;; expansion works normally
  (finishes
    (rmacroexpand
     '(with-let x 1 (limited-progn 1 2))))

  ;; the error is handled in compile time and ignored.
  (finishes
    (rmacroexpand
     '(with-let x 1 (limited-progn 1 2 3 4)))))


;; *Note* that the example below IGNORES the surrounding environment
;; in the expansion of the subforms,
;; because it does not give the &environment variable explicitly.

(defmacro env-checker (&environment env)
  `(quote ,(multiple-value-list (variable-information 'x env))))

(test recursive-macroexpansion-without-environment
  (is (tree-equal
       '(LET ((X 1))
         (DECLARE (TYPE INTEGER X))
         (LET ((Y X))
           '(NIL NIL NIL)))
       (rmacroexpand
        '(let ((x 1))
          (declare (type integer x))
          (with-let y x
            (env-checker)))))))


;; In order to pass the outer environment to the subform expander,
;; call =rmacroexpand= with optional second argument =env=.

(defexpand with-let-env (&environment env var-form value-form &body body)
  (handler-bind ((error #'stupid-error-handler))
    (rmacroexpand
     `(let ((,var-form ,value-form))
        ,@body)
     env)))

(test recursive-macroexpansion-without-environment
  (is (tree-equal
       '(LET ((X 1))
         (DECLARE (TYPE INTEGER X))
         (LET ((Y X))
           '(:LEXICAL T ((TYPE . INTEGER)))))
       (rmacroexpand
        '(let ((x 1))
          (declare (type integer x))
          (with-let-env y x
            (env-checker)))))))
