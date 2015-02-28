
(in-package :cl21-user)
(in-package :recursive-macroexpansion)

;; let,let*,flet,macrolet,symbol-macrolet

(defun expand-var (env %)
  (destructuring-bind (var def) %
    (list var (rmacroexpand def env))))

(defun expand-fn (env %)
  (destructuring-bind (name args . body) %
    `(,name ,args ,(rmacroexpand body env))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro %aug (env &rest args) `(augment-environment ,env ,@args)))

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
                                     (list name (enclose
                                                 (parse-macro name args body env)
                                                 env)))
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
