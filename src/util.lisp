
(in-package :cl21-user)
(in-package :recursive-macroexpansion)

;; *macroexpand-hook* = 'funcall by default


(defun take-env (args)
    (%take nil args))

(defun %take (acc rest)
  (match rest
    ((list* '&environment env rest)
     (assert (symbolp env) nil "&environment variable should be a symbol")
     (values env (append (reverse acc) rest)))
    ((list* thing nil)
     (signal "No &environment variable found")
     (values (gensym "ENV") (reverse (cons thing acc))))
    ((list* thing rest)
     (%take (cons thing acc) rest))))

;; (take-env '(a b &environment env d))

(defun take-declarations (body)
  (remove-if-not ^(eq 'declare (car %)) body))
