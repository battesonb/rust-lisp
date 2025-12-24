; A very basic standard library implemented in lisp, directly.

; Create extensions for the `<` and `>` comparisons
(setq <= (lambda (a b) (not (> a b))))
(setq >= (lambda (a b) (not (< a b))))
