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

(symbol-macrolet ((break (go out)))
  (tagbody
     (case <expr> (<val1> (go 1)) (<val2> (go 2) ...))
   1 <case1-code>
   2 <case-2-code>
   ...
   out))

(set-dispatch-macro-character
 #\# #\_
 (lambda (stream subchar arg)
   (parse-integer (remove #\_ (symbol-name (read stream nil nil t))))))

(defun fmfm (par-fmt &rest rest)
  (let ((fullstr (format nil "~~&~A" par-fmt)))
    (apply #'format t fullstr rest)))
