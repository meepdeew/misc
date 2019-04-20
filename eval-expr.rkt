(check-expect (evex "1+2") 3)
(check-expect (evex "1+22") 23)
(check-expect (evex "2-1") 1)
(check-expect (evex "2+-1") 1)
(check-expect (evex "-1--1") 0)
(check-expect (evex "1+2+3+4+5") 15)
(check-expect (evex "1+2-3+4-5") -1)
(check-expect (evex "-1--1--1") 1)
(check-expect (evex "-1--1+-1") -1)
(check-expect (evex "-1--1+-1--1") 0)
(check-expect (evex "1 + 1") 2)
(check-expect (evex "6-4 / 2") 4)
(check-expect (evex "2*5+5*2/3+6/2+8") 24)
(check-expect (evex "2+6* 3+5- 3*14/7+2*5+3") 32)
(check-expect (evex "1 + 2 * 3") 7)
(check-expect (evex "1 - 4 / 3") 0)
(check-expect (evex "-123") -123)
(check-expect (evex "1 / 3 - 1 - 23") -24)
(check-expect (evex "1 * 2 * 3 * 4 / 5") 4)
(check-expect (evex "1 + 123 / 123") 2)
(check-expect (evex "3 * 3 / 3 + 12") 15)
(check-expect (evex "2 * 3 + 4 * 5") 26)
(check-expect (evex "23 / 23 + 3 * 4 - 4 / 3") 12)
(check-expect (evex "6 / 3 / 2") 1)
(check-expect (evex "1 / 3 - -10 + 20") 30)
(check-expect (evex "1 / 5 - 10 + 20") 10)

;; String -> Number
(define (evex str)
  (evex-helper-add-sub
   (evex-helper-mul-div
    (join-as-necessary (rm-spaces (explode str))))
   0 "pos"))

(define (count-ops-helper lst acc)
  (cond [(empty? lst) acc]
        [else (count-ops-helper
               (cdr lst)
               (if (or (string=? (car lst) "*")
                       (string=? (car lst) "/"))
                   (+ 1 acc)
                   acc))]))

(check-expect (count-mul-div-ops (list "+")) 0)
(check-expect (count-mul-div-ops (list "*")) 1)
(check-expect (count-mul-div-ops (list "/")) 1)
(check-expect (count-mul-div-ops (list "*" "+")) 1)
(check-expect (count-mul-div-ops (list "-" "*")) 1)
(check-expect (count-mul-div-ops (list "*" "*")) 2)
(check-expect (count-mul-div-ops (list "*" "*" "*")) 3)
(check-expect (count-mul-div-ops (list "1" "*" "2")) 1)
(check-expect (count-mul-div-ops (list "1" "*" "2" "*" "3")) 2)
(check-expect (count-mul-div-ops (list "1" "*" "2" "*" "3" "*" "4")) 3)

;; (listof String) -> Number
(define (count-mul-div-ops lst)
  (count-ops-helper lst 0))


(check-expect (evex-md-once (list "1" "*" "2") empty)
              (list "2"))
(check-expect (evex-md-once (list "3" "+" "1" "*" "2") empty)
              (list "3" "+" "2"))
(check-expect (evex-md-once (list "3" "*" "1" "*" "2") empty)
              (list "3" "*" "2"))
(check-expect (evex-md-once (list "4" "+" "3" "*" "1" "*" "2") empty)
              (list "4" "+" "3" "*" "2"))
(check-expect (evex-md-once (list "4" "+" "3" "*" "2" "*" "1") empty)
              (list "4" "+" "6" "*" "1"))
(check-expect (evex-md-once (list "4" "+" "3" "/" "2" "*" "1") empty)
              (list "4" "+" "1" "*" "1"))



(define (cons-onto items lst)
  (cond [(empty? items) lst]
        [else (cons-onto (cdr items) (cons (car items) lst))]))

;; (listof String) -> (listof String)
(define (evex-md-once lst res)
  (cond [(< (length lst) 3)
         (if (empty? res)
             lst
             (reverse (cons-onto lst res)))]
        ;; multiply
        [(or (string=? (second lst) "*")
             (string=? (second lst) "/"))
         (evex-md-once (cdr (cdr (cdr lst)))
                       (cons (number->string
                              (if (string=? (second lst) "*")
                                  (* (string->number (first lst))
                                     (string->number (third lst)))
                                  (floor (/ (string->number (first lst))
                                     (string->number (third lst)))))) res))]
        ;; DNM
        [else
         (evex-md-once (cdr lst) (cons (car lst) res))]))



(check-expect (evex-helper-mul-div-times (list "1" "*" "2") 1)
              (list "2"))
(check-expect (evex-helper-mul-div-times (list "1" "*" "2" "*" "3") 2)
              (list "6"))
(check-expect (evex-helper-mul-div-times (list "1" "*" "2" "*" "3" "*" "4") 3)
              (list "24"))

(check-expect (evex-helper-mul-div-times (list "1") 0)
              (list "1"))
(check-expect (evex-helper-mul-div-times (list "1" "*" "2") 1)
              (list "2"))
(check-expect (evex-helper-mul-div-times (list "3" "+" "1" "*" "2") 1)
              (list "3" "+" "2"))

;; (listof String) Number -> (listof String)
(define (evex-helper-mul-div-times lst cnt)
  (cond [(= cnt 0) lst]
        [else (evex-helper-mul-div-times (evex-md-once lst empty) (- cnt 1))]))





(check-expect (evex-helper-mul-div (list "1" "*" "2"))
              (list "2"))
(check-expect (evex-helper-mul-div (list "1" "*" "2" "*" "3"))
              (list "6"))
(check-expect (evex-helper-mul-div (list "1" "*" "2" "*" "3" "*" "4"))
              (list "24"))

;; (listof String) Number -> (listof String)
(define (evex-helper-mul-div lst)
  (evex-helper-mul-div-times lst (count-mul-div-ops lst)))



(check-expect (evex-helper-add-sub (list "1" "+" "2") 0 "pos") 3)
(check-expect (evex-helper-add-sub (list "2" "-" "1") 0 "pos") 1)
(check-expect (evex-helper-add-sub empty 0 "pos") 0)
(check-expect (evex-helper-add-sub empty 2 "pos") 2)
(check-expect (evex-helper-add-sub (list "5") 0 "pos") 5)

;; (listof String) -> Number
(define (evex-helper-add-sub lst res sign)
  (cond [(empty? lst) res]
        ;; odd
        [(not (= (modulo (length lst) 2) 0))
         (evex-helper-add-sub (cdr lst)
                      (if (string=? sign "pos")
                            (+ res (string->number (car lst)))
                            (- res (string->number (car lst))))
                      sign)]
        [else (evex-helper-add-sub (cdr lst)
                           res
                           (cond [(string=? (car lst) "+") "pos"]
                                 [(string=? (car lst) "-") "neg"]))]))

(check-expect (join-as-necessary (list "1" "+" "2"))
              (list "1" "+" "2"))
(check-expect (join-as-necessary (list "1" "+" "2" "2"))
              (list "1" "+" "22"))
(check-expect (join-as-necessary (list "2" "-" "1"))
              (list "2" "-" "1"))
(check-expect (join-as-necessary (list "2" "+" "-" "1"))
              (list "2" "+" "-1"))
(check-expect (join-as-necessary (list "-" "1" "-" "-" "1"))
              (list "-1" "-" "-1"))
(check-expect (join-as-necessary (list "1" "+" "1"))
              (list "1" "+" "1"))
(check-expect (join-as-necessary (list "4" "/" "2"))
              (list "4" "/" "2"))
(check-expect (join-as-necessary (list "6" "-" "4" "/" "2"))
              (list "6" "-" "4" "/" "2"))


;; (listof String) -> (listof String)
(define (join-as-necessary lst)
  (join-negative-nums (join-digits lst)))

                   
(check-expect (join-digits (list "1"))
              (list "1"))
(check-expect (join-digits (list "1" "1"))
              (list "11"))
(check-expect (join-digits (list "2" "-" "1"))
              (list "2" "-" "1"))
(check-expect (join-digits (list "2" "+" "-" "1"))
              (list "2" "+" "-" "1"))
(check-expect (join-digits (list "2" "3" "4" "-" "1" "5" "6") )
              (list "234" "-" "156"))
(check-expect (join-digits (list "2" "2" "-" "1" "3" "1" "+" "4" "3" "2" "1"))
              (list "22" "-" "131" "+" "4321"))
(check-expect (join-digits (list "-" "1" "-" "-" "1"))
              (list "-" "1" "-" "-" "1"))
(check-expect (join-digits (list "4" "/" "2"))
              (list "4" "/" "2"))


;; (listof String) -> (listof String)
(define (join-digits lst)
  (reverse (join-digits-helper lst empty)))


(check-expect (join-digits-helper (list "2" "+" "1") empty)
              (list "1" "+" "2"))
(check-expect (join-digits-helper (list "+" "1") (list "2"))
              (list "1" "+" "2"))
(check-expect (join-digits-helper (list "1") (list "+" "2"))
              (list "1" "+" "2"))

;; (listof String) (listof String) -> (listof String)
(define (join-digits-helper lst res)
  (cond [(empty? lst) res]
        [(string=? (car lst) "+")
         (join-digits-helper (cdr lst) (cons (car lst) res))]
        [(string=? (car lst) "-")
         (join-digits-helper (cdr lst) (cons (car lst) res))]
        [(string=? (car lst) "*")
         (join-digits-helper (cdr lst) (cons (car lst) res))]
        [(string=? (car lst) "/")
         (join-digits-helper (cdr lst) (cons (car lst) res))]
        [(and (not (empty? res))
              (or (string=? (car res) "*") (string=? (car res) "/")
                  (string=? (car res) "+") (string=? (car res) "-")))
         (join-digits-helper (cdr lst) (cons (car lst) res))]
        [else
         (join-digits-helper (cdr lst)
                          (cons (string-append (if (empty? res) "" (car res))
                                               (car lst))
                                (if (empty? res)
                                    empty
                                    (cdr res))))]))


(check-expect (join-negative-nums (list "-" "7"))
              (list "-7"))
(check-expect (join-negative-nums (list "23" "+" "-" "7"))
              (list "23" "+" "-7"))
(check-expect (join-negative-nums (list "-" "1" "-" "-" "1"))
              (list "-1" "-" "-1"))
(check-expect (join-negative-nums (list "4" "/" "2"))
              (list "4" "/" "2"))

;; (listof String) -> (listof String)
(define (join-negative-nums lst)
  (reverse (join-neg-helper lst empty empty)))


(check-expect (insert-q2 empty "+") (list "+"))
(check-expect (insert-q2 (list "a") "+") (list "+" "a"))
(check-expect (insert-q2 (list "a" "b") "+") (list "+" "a"))

;; (listof String) String -> (listof String)
(define (insert-q2 q str)
  (cond [(empty? q) (cons str q)]
        [(= (length q) 1) (cons str q)]
        [(= (length q) 2) (cons str (cons (car q) empty))]))



(check-expect (join-neg-helper (list "2" "+" "1") empty empty) (list "1" "+" "2"))
(check-expect (join-neg-helper (list "-" "2" "+" "1") empty empty) (list "1" "+" "-2"))
(check-expect (join-neg-helper (list "2" "+" "-" "1") empty empty) (list "-1" "+" "2"))

;; (listof String) (listof String) -> (listof String)
;; if currently negtive and last thing was operator or nothing
(define (join-neg-helper lst res prev2)
  (cond [(empty? lst) res]
        [(and (= (length prev2) 1)
              (string=? (car prev2) "-"))
         (join-neg-helper (cdr lst)
                          (cons (string-append (car res) (car lst)) (cdr res))
                          (cons (car lst) empty))]
        [(and (= (length prev2) 2)
              (or (string=? (car (cdr prev2)) "-") (string=? (car (cdr prev2)) "+"))
              (string=? (car prev2) "-"))
         (join-neg-helper (cdr lst)
                          (cons (string-append (car res) (car lst)) (cdr res))
                          (cons (car lst) empty))]
        [else (join-neg-helper (cdr lst)
                               (cons (car lst) res)
                               (insert-q2 prev2 (car lst)))]))

(define (rm-spaces-helper lst acc)
  (cond [(empty? lst) acc]
        [(string=? (car lst) " ") (rm-spaces-helper (cdr lst) acc)]
        [else (rm-spaces-helper (cdr lst) (cons (car lst) acc))]))

(check-expect (rm-spaces (list "1" " " "+" " " "1"))
              (list "1" "+" "1"))
(check-expect (rm-spaces (list "6" "-" "4" " " "/" " " "2"))
              (list "6" "-" "4" "/" "2"))
(check-expect (rm-spaces (list "2" "*" "5" "+" "5" "*" "2" "/" "3" "+" "6" "/" "2" "+" "8"))
              (list "2" "*" "5" "+" "5" "*" "2" "/" "3" "+" "6" "/" "2" "+" "8"))
(check-expect (rm-spaces (list "2" "+" "6" "*" " " "3" "+" "5" "-" " " "3" 
                               "*" "1" "4" "/" "7" "+" "2" "*" "5" "+" "3"))
              (list "2" "+" "6" "*" "3" "+" "5" "-" "3" "*" 
                    "1" "4" "/" "7" "+" "2" "*" "5" "+" "3"))

(define (rm-spaces lst)
  (reverse (rm-spaces-helper lst empty)))
