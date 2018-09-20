;;;; PROBLEM DESCRIPTION

;;;; John loves to play an online game. He wants to know if opponents have the ultimate sword so that he can avoid fighting with such players. He has data of his friends and decides to make a classifier based on the data.

;;;; For each of his N friends (1 <= N <= 50,000), he knows the amount of the time a friend has played, and whether the friend have the item or not. Each of his friend have distinct amount of playtime. With the data, he builds a “nearest neighbor classifier”. For a new player P, first he finds a friend P’ in the data whose playtime is the closest to the playtime of P. If P’ has the item, he predicts that the new player P also has the item. If P’ does not have the item, he guessed that the new player P does not have the item. If there are more than one closest player among his friends, he guessed the new player P has the item if any one of them have it.

;;;; John carefully observes the possible opponents and finds out that there is one opponent of every integer playtime between A and B (inclusive). Please determine that how many of the opponents will have the ultimate sword, using the John’s classifier.

;;;; Note that the classifier guesses based on the information of the friends and does not use information on the new opponents. Also note that, A and B can be very large and your program may not be effective if you test each opponent between A and B against the entire friends. i.e. you will not want to simply iterate from A to B one by one and find the friend with the closest time among the entire dataset.

;;;; INPUT (read from a file named "input.txt")
;;;; The first line contains 3 integers N, A and B (1 <= A <= B <= 1,000,000,000).
;;;; The next N lines each describe one friend. Each line is either S T, meaning that the player played for time T has the sword, or NS T, meaning that the player played for time T doesn’t have the sword. Time T are all integers and 1 <= T <= 1,000,000,000.

;;;; SAMPLE INPUT: input.txt
;;;; 3 1 6
;;;; S  1
;;;; NS 4
;;;; S  6

;;;; OUTPUT (write to a file named "output.txt")
;;;; A single integer giving the number of opponents that the algorithm will classify as having the sword. In the example, opponents with playtime 1, 2, 5, 6 will be classified as having the sword. Therefore, the output will be 4.

;;;; SAMPLE OUTPUT: output.txt
;;;; 4

;;;; NOTE
;;;; The program should be implemented in either C, C++, Java or Python. The implementation should be efficient and it should be able to handle one input file within a second. If you are using Python, the program should take less than 3 seconds for one input.


(let* ((params '(:n 1 :a 1 :b 3))
       (data   '((1 y)))
       (n (getf params :n))
       (a (getf params :a))
       (b (getf params :b))
       (first-pair (first data))
       (final-pair (first (last data))))
  (if (not (= (first first-pair) a))
      (setf data (cons (list a (second first-pair)) data)))
  (if (not (= (first final-pair) b))
      (setf data (append data (list (list b (second final-pair))))))
  data)

;;; 2 1 3
;;; S  1
;;; NS 3
(floor (/ 5 2))
(ceiling (/ 5 2))


(let ((data '((1 y) (6 y)))
      (sword-count 0))
  (loop for (p1 p2) on data do
    (block loop-liner
      (let ((v1 (car p1))
            (v2 (car p2)))
        (when (equal (second p1) 'y) (incf sword-count))
        (when (null p2) (return-from loop-liner))
        (let ((num-btwn (- v2 v1 1))
              (vm (/ (+ v1 v2) 2)))
          (when (zerop num-btwn) (return-from loop-liner))

          (when (oddp num-btwn)
            (when (or (equal (second p1) 'y) (equal (second p2) 'y))
              (incf sword-count))
            (when (equal (second p1) 'y)
              (incf sword-count (- vm v1 1)))
            (when (equal (second p2) 'y)
              (incf sword-count (- v2 vm 1))))

          (when (evenp num-btwn)
            (when (equal (second p1) 'y)
              (incf sword-count (- (floor vm) v1)))
            (when (equal (second p2) 'y)
              (incf sword-count (- v2 (ceiling vm)))))

          )))) sword-count)

;;; 1 2         --> (zerop num-btwn)
'((1 n) (2 n));0
'((1 y) (2 n));1
'((1 n) (2 y));1
'((1 y) (2 y));2

;;; 1 _ 3       --> (oddp num-btwn)
'((1 n) (3 n));0
'((1 y) (3 n));2
'((1 n) (3 y));2
'((1 y) (3 y));3

;;; 1 _ _ 4     --> (evenp num-btwn)
'((1 n) (4 n));0
'((1 n) (4 y));2
'((1 y) (4 n));2
'((1 y) (4 y));4

;;; 1 _ _ _ 5   --> (oddp num-btwn)
'((1 n) (5 n));0
'((1 y) (5 n));3
'((1 n) (5 y));3
'((1 y) (5 y));5

;;; 1 _ _ _ _ 6 --> (evenp num-btwn)
'((1 n) (6 n));0
'((1 n) (6 y));3
'((1 y) (6 n));3
'((1 y) (6 y));6

;;; 1 _ _ 4 _ _ 7
'((1 y) (4 y) (7 n));4
