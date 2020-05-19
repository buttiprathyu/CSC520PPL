;; Name: Prathyusha Butti
;; Date due: April 8th, 2020
;; CSc 520 Homework 7: type systems
;; File: typed-funs.scm
;; Purpose: Contains uScheme code solutions 


;;============================================================
;;
;; Problem TD - Referred to Exercise 10 on Pg 195 from Scheme Homework
;; for drop and takewhile definitions
;;
;;============================================================
;; (a) 
;;============================================================
;; with predicate
;; (check-type drop (forall ('a) (('a -> bool)(list 'a) -> (list 'a))))

;; without predicate
(check-type drop (forall ('a) (int (list 'a) -> (list 'a))))

;;============================================================
;; (b) Definition of drop (not sure whether it was with predicate or not
;; so implemented both variations because it had reference to Scheme HW)
;; commented the one with predicate
;;============================================================
;; drop takes a list as argument, predicate and returns a list 
;; which drops the given number of elements from the front of the list

;;(val drop (type-lambda ('a)
;;    (letrec
;;        [([dropit : (('a->bool) (list 'a) -> (list 'a))]
;;            (lambda ([p? : ('a->bool)] [xs:(list 'a)])
;;                (if ((@ null? 'a) xs)
;;                    (@ '() 'a)
;;                    (if (p? ((@ car 'a) xs))
;;                        (dropit p? ((@ cdr 'a) xs))
;;                        xs))))]
;;        dropit)))
;;============================================================
;; drop takes a list as argument and returns a list 
;; which drops the given number of elements from the front of the list

(val drop (type-lambda ('a)
    (letrec
        [([dropit : (int (list 'a) -> (list 'a))]
            (lambda ([i : int] [xs:(list 'a)])
                (if ((@ null? 'a) xs)
                    (@ '() 'a)
                    (if (= i 0)
                       xs
                       (dropit (- i 1) ((@cdr 'a) xs))))))]
        dropit)))
;;============================================================
;; (c)
;;============================================================

(check-type takewhile (forall ('a) (('a -> bool)(list 'a) -> (list 'a))))

;;============================================================
;; (d) Definition of takewhile
;;============================================================
;; Takes as arguments a predicate f and a list 
;; returns the longest prefix where every element satisifies the predicate

(val takewhile (type-lambda ('a)
    (letrec
        [([take : (('a->bool) (list 'a) -> (list 'a))]
           (lambda ([p? : ('a->bool)] [xs:(list 'a)])
                (if ((@ null? 'a) xs)
                    (@ '() 'a)
                    (if (p? ((@ car 'a) xs))
                        ((@ cons 'a) ((@car 'a) xs) (take p? ((@ cdr 'a)xs)))
                        (@ '() 'a)))))]
    take)))
