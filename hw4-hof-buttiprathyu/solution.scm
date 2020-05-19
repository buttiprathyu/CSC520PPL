;; Name: Prathyusha Butti
;; Date due: 28th February 2020
;; CSc 520 Homework 4: hofs
;; File: solution.scm
;; Purpose: Contains uScheme code solutions 


;;============================================================
;;
;; Chapter 2, Problem 14 b
;;
;;============================================================
;; (max* xs) Takes as argument a non-empty integer list and
;; returns the maximum of the list

;; Function Definition:
(define max* (xs)
   (foldr (lambda (x n)
                  (if (> x n) x n)) (car xs) (cdr xs)))

;; Examples and tests:
(check-expect (max* '(1 2 7 6 4)) 7) ; standard case
(check-expect (max* '(1 1)) 1) ; duplicate case

;;============================================================
;;
;; Chapter 2, Problem 14 c
;;
;;============================================================
;; (gcd* xs) Takes as argument a non-empty integer list and
;; returns gcd of the elements in the list

;; Function Definition:
(define gcd* (xs)
   (foldr (lambda (x n) (gcd x n)) (car xs) (cdr xs)))

;; Examples and tests:
(check-expect (gcd* '(18 12 24 60 66)) 6) ; standard case
(check-expect (gcd* '(3 5 7 13 11)) 1) ; for prime numbers
(check-expect (gcd* '(5 5)) 5) ; duplicate elements in list
(check-expect (gcd* '(0 14)) 14) ; gcd is the largest element

;;============================================================
;;
;; Chapter 2, Problem 14 d
;;
;;============================================================
;; (lcm* xs) Takes as argument a non-empty integer list and
;; returns lcm of the elements in the list

;; Function Definition:
(define lcm* (xs)
   (foldr (lambda (x n) (lcm x n)) (car xs) (cdr xs)))

;; Examples and tests:
(check-expect (lcm* '(4 3 6)) 12) ; standard case
(check-expect (lcm* '(5 10 15)) 30) ; standard case
(check-expect (lcm* '(1 5)) 5) ; lcm is 5
(check-expect (lcm* '(0 1 4)) 0) ; lcm is zero in this case

;;============================================================
;;
;; Chapter 2, Problem 14 e
;;
;;============================================================
;; (sum xs) Takes as argument a non-empty integer list and
;; returns sum of the elements in the list

;; Function Definition:
(define sum (xs)
   (foldr + 0 xs))

;; Examples and tests:
(check-expect (sum '(1 2 3 4 5)) 15) ; standard case
(check-expect (sum '(-1 -2 0 1 2)) 0) ; standard case
(check-expect (sum '()) 0) ; empty list

;;============================================================
;;
;; Chapter 2, Problem 14 f
;;
;;============================================================
;; (product xs) Takes as argument a non-empty integer list and
;; returns product of the elements in the list

;; Function Definition:
(define product (xs)
   (foldr * 1 xs))

;; Examples and tests:
(check-expect (product '(1 2 3 4 5)) 120) ; standard case
(check-expect (product '(-1 -2 0 1 2)) 0) ; standard case
(check-expect (product '()) 1) ; empty list

;;============================================================
;;
;; Chapter 2, Problem 14 h
;;
;;============================================================
;; (append xs ys) Takes as argument two lists and
;; returns appended list of the two lists

;; Function Definition:
(define append (xs ys)
   (foldr cons ys xs))

;; Examples and tests:
(check-expect (append '(1 2 3) '(4 5 6)) '(1 2 3 4 5 6)) ; standard case
(check-expect (append '() '(0 1 2)) '(0 1 2)) ; one empty list
(check-expect (append '() '()) '()) ; two empty lists

;;============================================================
;;
;; Chapter 2, Problem 14 j
;;
;;============================================================
;; (reverse xs) Takes as argument a list and
;; returns reverse of the list

;; Function Definition:
(define reverse (xs)
   (foldl cons '() xs))

;; Examples and tests:
(check-expect (reverse '(1 (2 3))) '((2 3) 1)) ; association lists
(check-expect (reverse '(0 1 2)) '(2 1 0)) ; standard case
(check-expect (reverse '()) '()) ;  empty list

;;============================================================
;;
;; Chapter 2, Problem 15
;;
;;============================================================
;; (map f xs) Takes as argument a function and list
;; returns with function called on every element of the list

;; Function Definition:
(define map (f xs)
   (foldr (lambda (x n) (cons (f x) n)) '() xs))

;; Examples and tests:
(check-expect (map (lambda (x) (* x x)) '(1 2 3 4)) '(1 4 9 16)) ; standard
(check-expect (map (lambda (x) (> 3 x)) '(1 2 7 4)) '(#t #t #f #f))

;;============================================================

;; (filter f xs) Takes as argument a function and list
;; returns the list which satisfies the function

;;Function Definition:
(define filter (f xs)
   (foldr (lambda (x n) (if (f x) (cons x n) n)) '() xs))

;; Examples and tests:
(check-expect (filter (lambda (x) (> 3 x)) '(1 2 7 4)) '(1 2)) ; standard case
(check-expect (filter number? '(2 s 4 5)) '(2 4 5)) ; standard case

;;============================================================

;; (exists? f xs) Takes as argument a function and list
;; returns boolean value that satisfies the function

;;Function Definition:
(define exists? (f xs)
   (foldr (lambda (x n) (or (f x) n)) #f xs))

;; Examples and tests:
(check-expect (exists? (lambda (x) (> x 3)) '(7 4)) #t) ; true case
(check-expect (exists? (lambda (x) (< 3 x)) '(1 2)) #f) ; false case

;;============================================================

;; (all? f xs) Takes as argument a function and list
;; returns boolean value that satisfies the function

;;Function Definition:
(define all? (f xs)
   (foldr (lambda (x n) (and (f x) n)) #t xs))

;; Examples and tests:
(check-expect (all? (lambda (x) (> x 3)) '(1 2 7 4)) #f) ; true case
(check-expect (all? (lambda (x) (< x 3)) '(1 2)) #t) ; false case

;;============================================================
;;
;; Chapter 2, Problem 19 a
;;
;;============================================================
;; (evens xs) Takes as argument an element
;; returns a set which contains all the even integers

;; Function Definition:
(define evens (x)
    (= 0 (mod x 2)))

;; Examples and tests:
(check-expect (evens 3) #f) ;; check if 3 is even
(check-expect (evens 4) #t) ;; check if 4 is even

;;============================================================
;;
;; Chapter 2, Problem 19 b
;;
;;============================================================
;; (two-digits xs) Takes as argument an element
;; returns a set which contains all two-digit positive integers

;; Function Definition:
(define two-digits (x)
    (and (> x 9) (<= x 99)))

;; Examples and tests:
(check-expect (two-digits 9) #f) ;; check if 9 is two-digit
(check-expect (two-digits 93) #t) ;; check if 93 is two-digit
(check-expect (two-digits 999) #f) ;; check if 999 is two-digit

;;============================================================
;;
;; Chapter 2, Problem 19 c
;;
;;============================================================
;; (add-element x s) Takes as argument element and set s
;; and returns the set with in it

;; Function Definition:
(define add-element (x s)
    (lambda (y) (or (equal? y x) (s y))))

;; Examples and tests:
;; add 3 to set and check if 3 is in the set
(check-expect ((add-element 3 (lambda (x) #f)) 3) #t)
;; add 3 to set and check if 4 is in the set
(check-expect ((add-element 3 (lambda (x) #f)) 4) #f)

;;============================================================
;; (union s1 s2) Takes as argument sets s1, s2
;; and returns the union of sets

;; Function Definition:
(define union (s1 s2)
    (lambda (x) (or (s1 x) (s2 x))))

;; Examples and tests:
;; union of empty set and two-digits and check if 9 is in it
(check-expect ((union (lambda (x) #f) two-digits) 9) #f)
;; union of odd and even set and check if 10 is in it
(check-expect ((union (o not evens) evens) 10) #t)

;;============================================================
;; (inter s1 s2) Takes as argument sets s1, s2
;; and returns the intersection of sets

;; Function Definition:
(define inter (s1 s2)
    (lambda (x) (and (s1 x) (s2 x))))

;; Examples and tests:
;; inter of empty set and two-digits set and check if 9 is in it
(check-expect ((inter evens two-digits) 8) #f)
;; inter of odd and even set and check if 10 is in it
(check-expect ((inter evens two-digits) 10) #t)

;;============================================================
;; (diff s1 s2) Takes as argument sets s1, s2
;; and returns the set containing every element of s1
;; but not also in s2

;; Function Definition:
(define diff (s1 s2)
    (lambda (x) (and (s1 x) (not (s2 x)))))

;; Examples and tests:
;; diff between odd, evens and check if 81 is in it
(check-expect ((diff (o not evens) evens) 81) #t)
;; diff between odd, even and check if 28 is in it
(check-expect ((diff (o not evens) evens) 28) #f)

;;============================================================
;; Algebraic laws for member?

;; (member? x (add-element x s)) == #t, where (equal? y x)
;; (member? x (add-element y s)) == (s x), where (not (equal? y x))
;; (member? x (union s1 s2)) == (or (s1 x) (s2 x))
;; (member? x (inter s1 s2)) == (and (s1 x) (s2 x))
;; (member? x (diff  s1 s2)) == (and (s1 x) (not (s2 x)))
;;============================================================
;;(member? x s) copying from Pg 199
;; Takes as argument a element and function and
;; returns #t if that element satisfies being a member of the function
;; else returns #f

;; Function Definition:
(define member? (x s) (s x))

;; Examples and tests:
;; written to test the algebraic laws
(check-expect (member? 2 evens) #t) ; true case
(check-expect (member? 5 evens) #f) ; false case
(check-expect (member? 22 two-digits) #t) ; true case
(check-expect (member? 2 two-digits) #f) ; false case
(check-expect (member? 2 (add-element 3 evens)) #t) ; if not (equal? y x)
(check-expect (member? 3 (add-element 2 evens)) #f) ; if not (equal? y x)
(check-expect (member? 3 (add-element 3 evens)) #t) ; if (equal? y x)
(check-expect (member? 3 (union two-digits (o not two-digits))) #t)
(check-expect (member? 3 (union (lambda (x) #f) two-digits)) #f)
(check-expect (member? 22 (inter two-digits evens)) #t)
(check-expect (member? 21 (inter two-digits evens)) #f)
(check-expect (member? 390 (diff two-digits (o not two-digits))) #f)
(check-expect (member? 33 (diff two-digits (o not two-digits))) #t)

;;============================================================
;;
;; Problem A
;;
;;============================================================
;; (f-functional) uses a lambda to recursively behave like while loop
;; that modifies x until (p? x y) is satisfied and returns (h x y)

;; Function Definition:
(define f-functional (y)
   (letrec
        [(x e)
        (f (lambda (x) (if (p? x y)
                           (f (g x y))
                           (h x y))))]
        (f x)))

;; set x originally
;; then recursively set x using f
;; and return the result

;;============================================================
;;
;; Problem F
;;
;;============================================================
;; (flip f) Takes a function as argument and returns the function
;; which expects its arguments in the opposite order

;; Function Definition:
(define flip (f)
   (lambda (x y) (f y x)))

;; Algebraic law:
;; ((flip f) x y) == (f y x)

;; Examples and tests:
(check-expect ((flip <) 3 4) (< 4 3))
(check-expect ((flip append) '(1 4) '(5 6)) (append '(5 6) '(1 4)))
(check-expect ((flip (flip <)) 3 4) (< 3 4))

;;============================================================
;;
;; Problem O
;;
;;============================================================
;; (ordered-by? precedes?) Takes as argument one comparison function
;; and returns a predicate that tells if a list of values
;; is totally ordered by that relation

;; Examples and tests: Given in HW write up
(check-assert (procedure? ordered-by?))
(check-assert (procedure? (ordered-by? <)))
(check-error (ordered-by? < '(1 2 3)))
(check-expect ((ordered-by? <) '(1 2 3)) #t)
(check-expect ((ordered-by? <=) '(1 2 3)) #t)
(check-expect ((ordered-by? <) '(3 2 1)) #f)
(check-expect ((ordered-by? >=) '(3 2 1)) #t)
(check-expect ((ordered-by? >=) '(3 3 3)) #t)
(check-expect ((ordered-by? =) '(3 3 3)) #t)
(check-expect ((ordered-by? <) '()) #t)
(check-expect ((ordered-by? =) '(3)) #t)

;; Algebraic laws:
;; (ordered-by precedes? '()) == #t
;; (ordered-by precedes? '(x)) == #t
;; (ordered-by precedes? (cons x xs)) == (and (precedes? x)
;;                                            (ordered-by precedes? xs))

;; Function Definition:
(define ordered-by? (precedes?)
   (letrec
        ([order (lambda (xs)
                    (if (null? xs)
                         #t
                         (if (= (foldr (lambda (x n) (+ n 1)) 0 xs) 1)
                             #t
                             (if (precedes? (car xs) (car (cdr xs)))
                                 (order (cdr xs))
                                 #f))))])
        order))

