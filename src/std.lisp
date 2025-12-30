; A very basic standard library implemented in lisp, directly.

; Define sentinels for true and false
(setq t (quote t)) ; `t` is `t` (true)
(setq nil ())      ; `nil` is the empty list (false)

; Add function definition macro
(defmacro defun (name params body)
  (setq name (lambda params body)))

; `not` can be defined in terms of if
(defun not (a) (if a nil t))

; Create all extensions for the `<` and `=` comparisons.
(defun >= (a b) (not (< a b)))
(defun > (a b) (not (or (< a b) (= a b))))
(defun <= (a b) (or (< a b) (= a b)))
