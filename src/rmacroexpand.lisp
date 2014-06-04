
(in-package :cl21-user)
(in-package :recursive-macroexpansion)

(defun recursive-macro-function (symbol)
  (assert (symbolp symbol))
  (get symbol 'recursive-macro-function))

(defun (setf recursive-macro-function) (new-fn symbol)
  (assert (functionp new-fn))
  (setf (get symbol 'recursive-macro-function) new-fn))

(defmacro defexpand (name lambda-list &body body)
  (multiple-value-bind (whole sans-whole) (take-whole lambda-list)
    (multiple-value-bind (env sans-env-whole) (take-env sans-whole)
      `(eval-when (:compile-toplevel :load-toplevel :execute)
         (handler-case 
             (defmacro ,name (&whole ,whole
                              &environment ,env
                                ,@sans-env-whole)
               (rmacroexpand ,whole ,env))
           #+sbcl (SB-EXT:SYMBOL-PACKAGE-LOCKED-ERROR (c)
                    ;; abort redefinition
                    (declare (ignore c))))
         (setf (recursive-macro-function ',name)
               (,@(list #-sbcl 'lambda
                        #+sbcl 'sb-int:named-lambda
                        #+sbcl (intern (format nil "~S-EXPANDER" name)))
                  (,env ,whole ,@(subst '&rest '&body sans-env-whole))
                  (declare (ignorable ,env ,whole))
                  ,@body))))))

(defun rmacroexpand (form &optional env)
  ;; partly picked from macroexpand-1 in sbcl
  (cond ((consp form)
         ;; the form is either a special, macro, function, lambda form
         (if (symbolp (car form))
             (block nil
               (let ((head (car form)))
                 ;; recursive macro
                 (when-let1 (rmacrofn (recursive-macro-function head))
                   ;;(warn "recursive expander found")
                   (return (apply rmacrofn env form (cdr form))))
                 ;; standard macro
                 (when (macro-function head env)
                   ;;(warn "defmacro found; expanding recursively...")
                   ;; macroexpand-1 is necessary in order to override the normal macros
                   ;; Be sure that macroexpand-1 calls *macroexpand-hook*.
                   ;; If things are treated incorrectly, it will cause an infinite loop
                   (return (rmacroexpand (macroexpand-1 form env) env))))
               ;; special form or function form
               (return (rmacroexpand-core form env)))
             ;; cons, but the car is not a symbol = lambda form
             (rmacroexpand-lambda form env)))
        ;; form is atoms
        ((symbolp form) ;; symbol-macrolet, or variable
         (multiple-value-bind (expansion expanded-p) (macroexpand-1 form env)
           (if expanded-p
               (rmacroexpand expansion env)
               expansion)))
        ((constantp form) ;; self-evaluating object
         form)))

(defun rmacroexpand-lambda (form env)
  (map ^(rmacroexpand % env) form))

(defvar *special-forms-handlers* nil)

(defun rmacroexpand-core (form env)
  (if-let1 (h (getf *special-forms-handlers* (car form)))
    (apply h env (cdr form))
    `(,(car form) ,@(map ^(rmacroexpand % env) (cdr form)))))

(defmacro define-special-forms-handler (name args &body body)
  (multiple-value-bind (env args) (take-env args)
    `(progn
       (setf (getf *special-forms-handlers* ',name)
             #-sbcl
             (lambda (,env ,@args)
               ,@body)
             #+sbcl
             (sb-int:named-lambda ',(gensym (symbol-name name)) (,env ,@args)
                 ,@body))
       ',name)))


