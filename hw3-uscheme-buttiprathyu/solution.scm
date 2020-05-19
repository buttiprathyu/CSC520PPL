;; Name: Prathyusha Butti
;; Date due: 7th February 2020
;; CSc 520 Homework 3: uScheme
;; File: solution.scm
;; Purpose: Contains uScheme code solutions 


;;============================================================
;;
;; Chapter 2, Problem 10
;;
;;============================================================
;; (even? x) Takes as argument a non-negative integer and
;; returns #t if the number is even, else #f

;; Function Definition:
(define even? (x) (= (mod x 2) 0))

;; Examples and tests:
(check-expect (even? 68) #t) ; returns #t if x is even
(check-expect (even? 45) #f) ; returns #f if x is odd

;;============================================================

;; (takewhile p? xs) Takes as arguments a predicate f and a list xs,
;; returns the longest prefix of xs where every element satisfies the predicate

;; Examples and tests: From textbook Pg 195
;; standard case - return the matching longest predicte
;; base case - return empty if list is empty
(check-expect (takewhile even? '(2 4 6 7 8 10 12)) '(2 4 6)) ; standard case
(check-expect (takewhile even? '()) '()) ; base case

;; Function Definition:
(define takewhile (p? xs)
   (if (null? xs)
       '()
       ( if (p? (car xs))
           (append (list1 (car xs)) (takewhile p? (cdr xs)))
           '())))

;;============================================================
;; (dropwhile p? xs) Takes as arguments a predicate f and a list xs,
;; returns remaining of xs removing the longest predicate from the list

;; Examples and tests: From textbook Pg 195
;; standard case - return the remaining of list removing the longest predicate
;; base case - return empty if list is empty
(check-expect (dropwhile even? '(2 4 6 7 8 10 12)) '(7 8 10 12)); standard case
(check-expect (dropwhile even? '()) '()) ; base case

;; Exercise 37 which proves the algebraic law for takewhile, dropwhile
;; (append (takewhile p? xs) (dropwhile p? xs)) = xs
(check-expect (append (takewhile even? '(2 4 6 7 8))
                      (dropwhile even? '(2 4 6 7 8)))
                      '(2 4 6 7 8))

;; Function Definition:
(define dropwhile (p? xs)
(if (null? xs)
       '()
       ( if (p? (car xs))
           (dropwhile p? (cdr xs))
            xs)))

;;============================================================
;;
;; B. zip and unzip
;;
;;============================================================
;; (zip xs ys) Takes two lists as arguments and coverts the
;; pair of lists to an association list

;; Examples and tests:
;; standard case - converts pair of lists into associated list
;; base case - if list is empty it returns empty
(check-expect (zip '(1 2 3) '(a b c)) '((1 a) (2 b) (3 c))) ; standard case
(check-expect (zip '(1 2 3) '()) '()) ; base case

;; Function Definition:
(define zip (xs ys)
   (if (or (null? xs) (null? ys))
       '()
       (cons (list2 (car xs) (car ys)) (zip (cdr xs) (cdr ys)))))

;;============================================================
;; (unzip ps) Takes a list as an argument and converts the associated
;; list to a pair of lists

;; Examples and tests:
;; standard case - convert associated list to pair of lists
(check-expect (unzip '((1 a) (2 b) (3 c))) '((1 2 3) (a b c))) ; standard case
(check-expect (unzip '((I Magnin) (U Thant) (E Coli)))
                     '((I U E) (Magnin Thant Coli))) ; standard case

;; Function Definition:
(define unzip (ps)
    (list2 (map car ps) (map cadr ps)))

;; Checking the algebraic laws for zip and unzip
;; 1. (zip (car (unzip pairs)) (cadr (unzip pairs))) == pairs
(check-expect (zip (car (unzip '((1 a) (2 b) (3 c))))
              (cadr (unzip '((1 a) (2 b) (3 c)))))
              '((1 a) (2 b) (3 c)))

;; 2. (unzip (zip xs ys)) == (list2 xs ys)
(check-expect (unzip (zip '(1 2 3) '(a b c)))
              (list2 '(1 2 3) '(a b c)))
(check-expect (unzip (zip '() '())) (list2 '() '())) ; for empty lists


;;============================================================
;;
;; C. arg-max
;;
;;============================================================
;; (square a) Takes as argument a non-negative integer and returns
;; number multiplied by itself i.e. square of the number
;; Taken from homework 3 writeup

;; Function Definition:
(define square (a) (* a a))

;; Examples and tests:
(check-expect (square 5) 25) ; standard case returns 5*5

;;============================================================
;; (invert a) Takes as argument a non-negative integer and returns
;; 1000/a as the output
;; Taken from homework 3 writeup

;; Function Definition:
(define invert (a) (/ 1000 a))

;; Examples and tests:
(check-expect (invert 5) 200) ; standard case returns 1000/5

;;============================================================
;; (length xs) Takes as argument a list and returns the length of the list
;; Taken from text book Pg 102

;; Function Definition:
(define length (xs)
    (if (null? xs)
       0
       (+ 1 (length (cdr xs)))))

;; Examples and tests:
(check-expect (length '()) 0) ; base case if list is empty
(check-expect (length '(2 3 4)) 3) ; standard case if list not empty

;;============================================================
;; (arg-max f xs) Takes two arguments a function f and a non-empty list xs,
;; returns the value x in xs for which (f x) is as large as possible

;; Examples and tests:
(check-expect (arg-max square '(4 3 5 2 1)) 5) ; standard case for square
(check-expect (arg-max invert '(4 3 5 2 1)) 1) ; standard case for invert

;; Function Definition:
(define arg-max (f xs)
   (if (= 1 (length xs))
       (car xs)
       ( if (>= (f (car xs)) (f (arg-max f (cdr xs))))
           (car xs)
           (arg-max f (cdr xs)))))

;;============================================================
;;
;; D. permutation?
;;
;;============================================================
;; Idea for permutation? from stack overflow
;; https://stackoverflow.com/questions/23635911/
;;                             permutation-in-scheme-using-recursion

;; (permuteHelper ys str) Takes as arguments a list and a character to be
;; compared with the list, it returns #t if the character is present, else #f

;; Examples and tests:
;; standard case - when the element is present in the list return #t, else #f
;; base case - when the list is empty
(check-expect (permuteHelper '(a b c) 'c) #t) ; standard #t case
(check-expect (permuteHelper '(a b c) 'd) #f) ; standard #f case
(check-expect (permuteHelper '() 's) #f) ; base case

;; Function Definition:
(define permuteHelper (xs ch)
   (if (null? xs)
       #f
       (if (= (car xs) ch)
           #t
           (permuteHelper (cdr xs) ch))))

;;============================================================
;; (remove xs ch) Takes as arguments a list and the character to be
;; removed from the list, it returns a list with the character removed

;; Examples and tests:
;; standard case - when element is removed from the list
;; base case - if the list is empty
(check-expect (removeHelper '(a b c) 'b) '(a c)) ; standard case
(check-expect (removeHelper '() 'c) '()) ; base case

;; Function Definition:
(define removeHelper (xs ch)
   (if (null? xs)
       '()
       (if (= (car xs) ch)
           (removeHelper (cdr xs) ch)
           (cons (car xs) (removeHelper (cdr xs) ch)))))

;;============================================================
;; (permutation? xs ys) Takes as arguments two lists and returns #t
;; if the two lists are permutations i.e if both the lists are of
;; same length and have exactly the same elements or returns #f if they are
;; not permutations

;; Examples and tests:
;; standard case - if both lists are permutations
(check-expect (permutation? '(a b c) '(c b a)) #t) ; standard case
(check-expect (permutation? '(a b c) '(b a c)) #t) ; standard case
(check-expect (permutation? '(a b c) '(b c)) #f) ; lengths are not same
(check-expect (permutation? '(a b b) '(a a b)) #f) ; elements are not same
(check-expect (permutation? '(a b c) '(c b a d)) #f) ; lengths are not same

;; Function Definition:
(define permutation? (xs ys)
    (if (!= (length xs) (length ys))
        #f
        (if (null? ys)
            #t
            (if (permuteHelper xs (car ys))
              (permutation? (removeHelper xs (car ys)) (cdr ys))
              #f))))


