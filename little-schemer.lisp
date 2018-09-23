;;; Scheme features: car, cdr, cons, eq?, null?, zero?, add!, sub!, number?, and, or, quote, lambda, define, and cond.

(defun atom? (x)
  (and (not (consp x))
       (not (null x))))

(equal (atom? (quote ())) nil)
