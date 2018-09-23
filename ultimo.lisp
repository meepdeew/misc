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

(defun main (&optional (filename "./input.txt"))
  (with-open-file (stream filename)
    (let ((n (read stream))
          (a (read stream))
          (b (read stream)))
      (let ((params (list :n n :a a :b b))
            (data (loop for i upto (- n 1)
                        collect
                        (let ((w-row (if (equal (read stream) 'NS) 'n 'y))
                              (v-row (read stream)))
                          (list v-row w-row)))))
        ;; (format t  "~&headers ~S~%" params)
        ;; (format t "~&original data:~%~&~S~%" data)
        (check-and-pad params data)))))

(defun check-and-pad (params data)
  (let* (;; (n (getf params :n))
         (a (getf params :a))
         (b (getf params :b))
         (first-pair (first data))
         (final-pair (first (last data))))
    ;; TODO: make sure data input is sorted
    ;; TODO: check no rows outside A..B before padding
    (if (not (= (first first-pair) a))
        (setf data (cons (list a (second first-pair)) data)))
    (if (not (= (first final-pair) b))
        (setf data (append data (list (list b (second final-pair))))))
    ;; (format t "~&updated data:~%~&~S~%" data)
    (count-swords data)))

(defun count-swords (data)
  (let ((scount 0))
    (loop for (p1 p2) on data do
      (block loop-liner
        (let ((v1 (car p1))
              (v2 (car p2))
              (w1 (second p1))
              (w2 (second p2)))
          (when (equal (second p1) 'y) (incf scount))
          (when (null p2) (return-from loop-liner))
          (let ((num-btwn (- v2 v1 1))
                (vm (/ (+ v1 v2) 2)))
            (when (zerop num-btwn) (return-from loop-liner))
            (when (oddp num-btwn)
              (when (or (equal w1 'y) (equal w2 'y))
                (incf scount))
              (when (equal w1 'y)
                (incf scount (- vm v1 1)))
              (when (equal w2 'y)
                (incf scount (- v2 vm 1))))
            (when (evenp num-btwn)
              (when (equal w1 'y)
                (incf scount (- (floor vm) v1)))
              (when (equal w2 'y)
                (incf scount (- v2 (ceiling vm)))))))))
    ;; (format t "~&Number of opponents with ultimate sword: ~S~%" scount)
    scount))

(defparameter test-cases
  '(("./tests/input1.txt"  0) ("./tests/input2.txt"  1)
    ("./tests/input3.txt"  1) ("./tests/input4.txt"  2)
    ("./tests/input5.txt"  0) ("./tests/input6.txt"  2)
    ("./tests/input7.txt"  2) ("./tests/input8.txt"  3)
    ("./tests/input9.txt"  0) ("./tests/input10.txt" 2)
    ("./tests/input11.txt" 2) ("./tests/input12.txt" 4)
    ("./tests/input13.txt" 0) ("./tests/input14.txt" 3)
    ("./tests/input15.txt" 3) ("./tests/input16.txt" 5)
    ("./tests/input17.txt" 0) ("./tests/input18.txt" 3)
    ("./tests/input19.txt" 3) ("./tests/input20.txt" 6)
    ("./tests/input21.txt" 0) ("./tests/input22.txt" 2)
    ("./tests/input23.txt" 2) ("./tests/input24.txt" 1)
    ("./tests/input25.txt" 3) ("./tests/input26.txt" 3)
    ("./tests/input27.txt" 3) ("./tests/input28.txt" 4)
    ("./tests/input29.txt" 0) ("./tests/input30.txt" 1)
    ("./tests/input31.txt" 2) ("./tests/input32.txt" 2)
    ("./tests/input33.txt" 3) ("./tests/input34.txt" 3)
    ("./tests/input35.txt" 3) ("./tests/input36.txt" 4)))

(loop for test-case in test-cases do
  (let ((fname (first test-case))
        (expected (second test-case)))
    (format t "~&~A is ~A~%" fname (= (main fname) expected))))
