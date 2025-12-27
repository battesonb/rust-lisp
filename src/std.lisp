; A very basic standard library implemented in lisp, directly.

; `not` can be defined in terms of if
(setq not (lambda (a) (if a () t)))

; Create all extensions for the `<` and `=` comparisons.
(setq >= (lambda (a b) (not (< a b))))
(setq > (lambda (a b) (not (or (< a b) (= a b)))))
(setq <= (lambda (a b) (or (< a b) (= a b))))
