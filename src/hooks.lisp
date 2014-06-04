
(in-package :cl21-user)
(in-package :recursive-macroexpansion)

(define-symbol-plist-accessor special-form-hooks)

;; add hooks that are called when
;; rmacroexpand find a special form and recurse into that branch of source code

(eval-when (:compile-toplevel
            :load-toplevel
            :execute)
  (defmacro define-hook (name target (form env) &body body)
    `(setf (special-form-hooks ',target)
           (maybe-named-lambda ',name (,form ,env)
             ,@body))))
