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


(set-dispatch-macro-character
 #\# #\_
 (lambda (stream subchar arg)
   (parse-integer (remove #\_ (symbol-name (read stream nil nil t))))))

(defmacro fmtf (out par-fmt &rest args)
  (let ((fstr (format nil "~&~A~%" par-fmt)))
    `(format ,out ,fstr ,@args)))
