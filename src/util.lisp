
(in-package :cl21-user)
(in-package :recursive-macroexpansion)

;; *macroexpand-hook* = 'funcall by default

(defun take-whole (lambda-list)
  (if (eq (first lambda-list) '&whole)
      (values (second lambda-list) (cddr lambda-list))
      (values (gensym "WHOLE") lambda-list)))

(defun take-env (lambda-list)
  (%take nil lambda-list))

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
  (mappend #'cdr
           (keep-if #'(and consp ^(eq 'declare (car %)))
                    body)))
