; A very basic standard library implemented in lisp, directly.

; Define sentinels for true and false
(setq t (quote t)) ; `t` is `t` (true)
(setq nil ())      ; `nil` is the empty list (false)

; Add function definition macro
(defmacro defun (name params body)
  (setq name (lambda params body)))

; `not` can be defined in terms of if
(defun not (a) (if a nil t))

(defun null (x) (= nil x))

; Create all extensions for the `<` and `=` comparisons.
(defun >= (a b) (not (< a b)))
(defun > (a b) (not (or (< a b) (= a b))))
(defun <= (a b) (or (< a b) (= a b)))

; Bootstrap all variants of car and cdr
(defun caar   (lst) (car (car lst)))
(defun caaar  (lst) (car (car (car lst))))
(defun caaaar (lst) (car (car (car (car lst)))))

(defun cddr   (lst) (cdr (cdr lst)))
(defun cdddr  (lst) (cdr (cdr (cdr lst))))
(defun cddddr (lst) (cdr (cdr (cdr (cdr lst)))))

(defun cadr   (lst) (car (cdr lst)))
(defun caddr  (lst) (car (cdr (cdr lst))))
(defun cadddr (lst) (car (cdr (cdr (cdr lst)))))

(defun cdar   (lst) (cdr (car lst)))
(defun cdadr  (lst) (cdr (car (cdr lst))))
(defun cdaddr (lst) (cdr (car (cdr (cdr lst)))))

(defun cddar  (lst) (cdr (cdr (car lst))))
(defun cddadr (lst) (cdr (cdr (car (cdr lst)))))

(defun cdddar (lst) (cdr (cdr (cdr (car lst)))))

; Define associative array "get" function
(defun assoc (lst value)
  (if (null lst) nil
    (let ((head (car lst))
          (rest (cdr lst)))
      (if (= head nil) nil
        (let ((key (car head)))
          (if (= key value) head
            (assoc rest value)))))))

; Define the list map function
(defun mapcar (func_symbol lst)
  (if (null lst) nil
    (let ((head (car lst))
          (rest (cdr lst)))
      (cons
        (eval (list func_symbol head))
        (mapcar func_symbol rest)))))

; Define the list reduce function
(defun reduce (func_symbol lst)
  (if (null lst) nil
    (let ((a (car lst))
          (rest (cdr lst)))
      (if (null rest) a
        (let ((b (car rest))
              (trail (cdr rest)))
          (reduce func_symbol (cons (eval (list func_symbol a b)) trail)))))))
