; A very basic standard library implemented in lisp, directly.

; Define sentinels for true and false
(setq t (quote t)) ; `t` is `t` (true)
(setq nil ())      ; `nil` is the empty list (false)

; `not` can be defined in terms of if
(setq not (lambda (a) (if a () t)))

; Create all extensions for the `<` and `=` comparisons.
(setq >= (lambda (a b) (not (< a b))))
(setq > (lambda (a b) (not (or (< a b) (= a b)))))
(setq <= (lambda (a b) (or (< a b) (= a b))))

; Add function definition macro
(defmacro defun (name params body)
  (setq name (lambda params body)))
