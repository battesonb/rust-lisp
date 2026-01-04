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
  (if lst
    (let ((head (car lst)))
      (if head
        (if (= (car head) value) head
          (assoc (cdr lst) value))
        nil))
    nil))

; Define the list map function
(defun mapcar (func lst)
  (if lst
    (cons
      (eval (list func (car lst)))
      (mapcar func (cdr lst)))
    nil))

; Define the list filter function
(defun filter (func lst)
  (if lst
    (let ((head (car lst)))
      (if (eval (list func head))
        (cons head (filter func (cdr lst)))
        (filter func (cdr lst))))
    nil))

; Define the list reduce function. The func parameter can either be a Lambda or a symbol
; representing a Lambda bound to a variable.
(defun reduce (func lst)
  (if lst
    (let ((a (car lst))
          (rest (cdr lst)))
      (if rest
        (let ((b (car rest))
              (trail (cdr rest)))
          (reduce func (cons (eval (list func a b)) trail)))
        a))
    nil))

; Gets the nth element from a linked list in O(n) time.
(defun nth (index lst)
  (cond ((null lst) nil)
        ((< index 0) nil)
        ((= 0 index) (car lst))
        (t (nth (- index 1) (cdr lst)))))

; Type predicate, checks if a value matches a given type
(defun typep (value type)
  (= (type-of value) type))
