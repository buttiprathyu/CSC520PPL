;; Name: Prathyusha Butti
;; Date due: March 7th, 2020
;; CSc 520 Homework 5: continuations
;; File: solution.scm
;; Purpose: Contains uScheme code solutions 


;;============================================================
;;
;; Problem F
;;
;;============================================================
;; (formula? f) Takes as argument an arbitrary S-Expression and
;; returns #t if the S-Expression is a boolean formula
;; else #f otherwise

;; Function Definition:
(define formula? (f)
    (if (symbol? f)
        #t
        (if (equal? 'not (car f))
            (if (null? (cddr f))
                (formula? (cadr f))
                #f)
            (if (equal? 'and (car f))
                (if (null? (cddr f))
                     #f
                     (and (formula? (cadr f)) (formula? (caddr f))))
                (if (equal? 'or (car f))
                    (if (null? (cddr f))
                         #f
                         (or (formula? (cadr f)) (formula? (caddr f))))
                    (formula? (car f)))))))

;; Examples and tests:
(check-expect (formula? '(b a)) #t) ; false symbol case
(check-expect (formula? 'a) #t) ; true symbol case
(check-expect (formula? '(not a)) #t) ; true not case
(check-expect (formula? '(not (not a))) #t) ; true not case
(check-expect (formula? '(not a b)) #f) ; false not case
(check-expect (formula? '(and a b)) #t) ; true and case
(check-expect (formula? '(and (and a b) b)) #t) ; true and case
(check-expect (formula? '(and (and a))) #f) ; false and case
(check-expect (formula? '(and (not a) a)) #t) ; and-not true case
(check-expect (formula? '(and (not a))) #f) ; and-not false case
(check-expect (formula? '(or a b)) #t); true or case
(check-expect (formula? '(or (or a b) b)) #t) ; true or case
(check-expect (formula? '(or (or a))) #f) ; false or case
(check-expect (formula? '(or (not a) a)) #t) ; or-not true case
(check-expect (formula? '(or (not a))) #f) ; or-not false case
(check-expect (formula? '(and (or a b) (not a))) #t) ; and-or-not case
(check-expect (formula? '(or (and a b) (not a))) #t) ; or-and-not case
(check-error (formula? '(5 6)))
(check-error (formula? '#t))

;;============================================================
;;
;; Chapter 2, Problem 21
;;
;;============================================================
;; (find-formula-true-asst f fail succeed) Takes as arguments formula f,
;; failure continuation, success continuation and returns a satisfying
;; assignment for formula f

;; Function Definition:
(define find-formula-true-asst (f fail succeed)
    (letrec
       ([find-formula-asst (lambda (formula bool cur fail succeed)
            (if (atom? formula)
                (if (null? (find formula cur))
                    (succeed (bind formula bool cur) fail)
                    (if (equal? (find formula cur) bool)
                        (succeed cur fail)
                        (fail)))
                (if (equal? 'not (car formula))
                    (find-formula-asst (cadr formula)
                                       (not bool) cur fail succeed)
                    (if (equal? 'and (car formula))
                        (if (equal? #t bool)
                            (find-all-asst (cdr formula)
                                            bool cur fail succeed)
                            (find-any-asst (cdr formula)
                                            bool cur fail succeed))
                        (if (equal? 'or (car formula))
                            (if (equal? #t bool)
                                (find-any-asst (cdr formula)
                                               bool cur fail succeed)
                                (find-all-asst (cdr formula)
                                               bool cur fail succeed))
                            (find-formula-asst (car formula)
                                               bool cur fail succeed))))))]
        [find-any-asst (lambda (formulas bool cur fail succeed)
            (if (null? formulas)
                (fail)
                (find-formula-asst (car formulas) bool cur
                     (lambda () (find-any-asst (cdr formulas)
                                      bool cur fail succeed)) succeed)))]
        [find-all-asst (lambda (formulas bool cur fail succeed)
            (if (null? formulas)
                (succeed cur fail)
                (find-formula-asst (car formulas) bool cur fail
                     (lambda (cur resume) (find-all-asst (cdr formulas)
                                           bool cur resume succeed)))))])
        (find-formula-asst f #t '() fail succeed)))

;; Examples and tests:
;; standard OR case
(check-expect
      (find-formula-true-asst '(or a b)
                               (lambda () 'fail)
                               (lambda (cur resume) cur))
      '((a #t)))
;; standard AND case
(check-expect
      (find-formula-true-asst '(and a b)
                               (lambda () 'fail)
                               (lambda (cur resume) cur))
      '((a #t) (b #t)))
;; double negation case
(check-expect
      (find-formula-true-asst '(not (not a))
                               (lambda () 'fail)
                               (lambda (cur resume) cur))
      '((a #t)))
;; de-morgan's law version 1
(check-expect
      (find-formula-true-asst '(not (and a b))
                               (lambda () 'fail)
                               (lambda (cur resume) cur))
      '((a #f)))
;; de-morgan's law version 2
(check-expect
      (find-formula-true-asst '(not (and b (not a)))
                               (lambda () 'fail)
                               (lambda (cur resume) cur))
      '((b #f)))
;; de-morgan's law version 3
(check-expect
      (find-formula-true-asst '(not (or a b))
                               (lambda () 'fail)
                               (lambda (cur resume) cur))
      '((a #f) (b #f)))
;; complex case 1
(check-expect
    (find-formula-true-asst '(or (not (and (or a b) c)) c)
                             (lambda () 'fail)
                             (lambda (cur resume) cur))
    '((a #f) (b #f)))
;; complex case 2
(check-expect
     (find-formula-true-asst '(and (or (not a) b) (not (or b c)))
                              (lambda () 'fail)
                              (lambda (cur resume) cur))
     '((a #f) (b #f) (c #f)))
;; complex case 3
(check-expect
     (find-formula-true-asst '(not (or a (and b (not c))))
                              (lambda () 'fail)
                              (lambda (cur resume) cur))
     '((a #f) (b #f)))
;; no-solution complex case 1
(check-expect
     (find-formula-true-asst '(or (and (not a) (and b a)) (and a (not a)))
                              (lambda () 'fail)
                              (lambda (cur resume) cur))
     'fail)
;; no-solution testing Exercise T - f1
(check-expect
     (find-formula-true-asst '(and (or a (not b)) (and (not a) b))
                              (lambda () 'fail)
                              (lambda (cur resume) cur))
     'fail)
;; testing Exercise T - f2
(check-expect
     (find-formula-true-asst '(and (not (or x (and y z)))
                              (or (not (and x y (or z x)))
                              (and (not x) y z)))
                              (lambda () 'fail)
                              (lambda (cur resume) cur))
     '((x #f) (y #f)))
;; testing Exercise T - f3
(check-expect
     (find-formula-true-asst '(not (or x (and y (not x))))
                              (lambda () 'fail)
                              (lambda (cur resume) cur))
     '((x #f) (y #f)))

;; testing Pg 149 case
(check-expect
     (find-formula-true-asst '(and (or x y z)
                                   (or (not x) (not y) (not z))
                                   (or x y (not z)))
                              (lambda () 'fail)
                              (lambda (cur resume) cur))
     '((x #t) (y #f)))

(use solver-interface-tests.scm)

