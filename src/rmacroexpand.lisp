
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
         (defmacro ,name (&whole ,whole
                          &environment ,env
                            ,@sans-env-whole)
           (rmacroexpand ,whole ,env))
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


;; let,let*,flet,macrolet,symbol-macrolet

(defun expand-var (env %)
  (destructuring-bind (var def) %
    (list var (rmacroexpand def env))))

(defun expand-fn (env %)
  (destructuring-bind (name args . body) %
    `(,name ,args ,(rmacroexpand body env))))

(defmacro %aug (env &rest args) `(augment-environment ,env ,@args))

(define-special-forms-handler let (&environment env bindings &rest body)
  (let ((newenv (%aug env
                      :variable (map #'car bindings)
                      :declare (take-declarations body))))
    `(let ,(map ^(expand-var env %) bindings)
       ,@(map ^(rmacroexpand % newenv) body))))

(define-special-forms-handler let* (&environment env bindings &rest body)
  (let ((newenv (%aug env
                      :variable (map #'car bindings)
                      :declare (take-declarations body))))
    `(let* ,(map ^(expand-var newenv %) bindings)
       ,@(map ^(rmacroexpand % newenv) body))))

(define-special-forms-handler flet (&environment env bindings &rest body)
  (let ((newenv (%aug env
                      :function (map #'car bindings)
                      :declare (take-declarations body))))
    `(flet ,(map ^(expand-fn env %) bindings)
       ,@(map ^(rmacroexpand % newenv) body))))

(define-special-forms-handler labels (&environment env bindings &rest body)
  (let ((newenv (%aug env
                      :function (map #'car bindings)
                      :declare (take-declarations body))))
    `(labels ,(map ^(expand-fn newenv %) bindings)
       ,@(map ^(rmacroexpand % newenv) body))))

(define-special-forms-handler macrolet (&environment env bindings &rest body)
  (let ((newenv (%aug env
                      :macro (map ^(destructuring-bind (name args &rest body) %
                                     (list name (parse-macro name args body env)))
                                  bindings))))
    `(progn ,@(map ^(rmacroexpand % newenv)
                   body))))

(define-special-forms-handler symbol-macrolet (&environment env bindings &rest body)
  (let ((newenv (%aug env :symbol-macro bindings)))
    `(progn ,@(map ^(rmacroexpand % newenv) body))))

(define-special-forms-handler locally (&environment env &rest body)
  `(progn
     ,@(map ^(rmacroexpand % (%aug env :declare (take-declarations body)))
            body)))

;; special
(define-special-forms-handler tagbody (&environment env &rest body)
  `(progn
     ,@(map ^(if (not (consp %)) %
                 (rmacroexpand % (%aug env :declare (take-declarations body))))
            body)))

;; (X arg &rest rest) -- 1st argument not evaluated
(macrolet ((h (special)
             `(define-special-forms-handler ,special (&environment env arg &rest rest)
                `(,',special ,arg ,@(map ^(rmacroexpand % env) rest)))))
  (h block)
  (h go)
  (h eval-when)
  (h quote)
  (h return-from)
  (h function)
  (h setq))

;; (X &rest rest) -- rest evaluated
(macrolet ((h (special)
             `(define-special-forms-handler ,special (&environment env &rest rest)
                `(,',special ,@(map ^(rmacroexpand % env) rest)))))
  (h catch)
  (h if)
  (h multiple-value-call)
  (h multiple-value-prog1)
  (h progn)
  (h progv)
  (h the)
  (h throw)
  (h unwind-protect)
  (h load-time-value))




