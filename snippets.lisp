(defun fact-rec (n)
  (if (zerop n)
      1
      (* n (fact-rec (- n 1)))))

(defun fact-cps (n &optional (k #'values))
  (if (zerop n)
      (funcall k 1)
      (fact-cps (- n 1)
                (lambda (v)
                  (funcall k (* v n))))))

(loop ... do
  (block continue
    ...
    (return-from continue)
    ...))

(loop named loop ... do
  (tagbody retry ...
     (go continue) ...
     (go retry) ...
     (return-from loop) ...
   continue))
