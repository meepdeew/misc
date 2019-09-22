;;;; Emacs function (execute like eval C-x C-e) to incrementally move (in-place?) (recursively?) operator position within the form. Shouldn't matter whether infix notation, prefix, etc.

(defun count-fns (lst)
  (apply '+ (mapcar (lambda (el)
		      (if (functionp el) 1 0))
		    lst)))

(defun extract-fn/pos-helper (opr opr-idx cur-idx els-pre els-rem)
  (cond ((null els-rem)
	 (list opr opr-idx (reverse els-pre)))
	((functionp (car els-rem))
	 (extract-fn/pos-helper
	  (car els-rem) cur-idx (+ 1 cur-idx) els-pre (cdr els-rem)))
	(t (extract-fn/pos-helper
	    opr opr-idx (+ 1 cur-idx) (cons (car els-rem) els-pre) (cdr els-rem)))))

(defun extract-fn/pos (lst)
  (extract-fn/pos-helper nil -1 0 nil lst))

(defun insert-fn/pos-helper (opr idx els-pre els-rem)
  (cond ((and (= idx 0) (null els-pre))
	 (cons opr els-rem))
	((and (= idx 0) (not (null els-pre)))
	 (append (reverse els-pre) (cons opr els-rem)))
	(t (insert-fn/pos-helper
	    opr (- idx 1) (cons (car els-rem) els-pre) (cdr els-rem)))))

(defun insert-fn/pos (opr idx els)
  (if (> idx (length els))
      nil (insert-fn/pos-helper opr idx nil els)))

(defun mv (lst direction)
  (when (= (count-fns lst) 1)
    (let* ((extracted (extract-fn/pos lst))
	   (opr (car extracted))
	   (oid (cadr extracted))
	   (nid (cond ((string= "right" direction) (+ oid 1))
		      ((string= "left" direction) (- oid 1))))
	   (loa (caddr extracted)))
      (if (and (<= nid (length loa))
	       (>= nid 0))
	  (insert-fn/pos opr nid loa)
	lst))))

(defun shift-func-right-handler ()
  (interactive)
  (backward-kill-sexp)
  (let ((prev-form (read (current-kill 0))))
    (prin1 (mv prev-form "right") (current-buffer))))

(defun shift-func-left-handler ()
  (interactive)
  (backward-kill-sexp)
  (let ((prev-form (read (current-kill 0))))
    (prin1 (mv prev-form "left") (current-buffer))))

(global-set-key (kbd "C-c <right>") 'shift-func-right-handler)
(global-set-key (kbd "C-c <left>") 'shift-func-left-handler)

(mv '(+ 2 3) "right") ;; (2 + 3)
(mv '(2 + 3) "right") ;; (2 3 +)
(mv '(2 3 +) "right") ;; (2 3 +)

(mv '(2 3 +) "left") ;; (2 + 3)
(mv '(2 + 3) "left") ;; (+ 2 3)
(mv '(+ 2 3) "left") ;; (+ 2 3)

(count-fns '('+ '2 '3))
(count-fns '(+ 2 3))

(functionp '+)  ;; t
(functionp ''+) ;; nil

(insert-fn/pos '+ '0 '(2 3)) ;; (+ 2 3)
(insert-fn/pos '+ '1 '(2 3)) ;; (2 + 3)
(insert-fn/pos '+ '2 '(2 3)) ;; (2 3 +)
(insert-fn/pos '+ 2 '(2 3)) ;; (2 3 +)
(insert-fn/pos '+ 3 '(2 3)) ;; nil

(extract-fn/pos '(+ 2 3)) ;; (+ 0 (2 3))
(extract-fn/pos '(2 + 3)) ;; (+ 1 (2 3))
(extract-fn/pos '(2 3 +)) ;; (+ 2 (2 3))

(insert-fn/pos '+ '0 '(2 3)) ;; (+ 2 3)
(insert-fn/pos '+ '1 '(2 3)) ;; (2 + 3)
(insert-fn/pos '+ '2 '(2 3)) ;; (2 3 +)
(insert-fn/pos '+ 2 '(2 3)) ;; (2 3 +)
(insert-fn/pos '+ 3 '(2 3)) ;; nil


