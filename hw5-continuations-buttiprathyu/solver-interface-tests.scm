
    (check-expect (procedure? find-formula-true-asst) #t) ; correct name
    (check-error (find-formula-true-asst))                ; not 0 arguments
    (check-error (find-formula-true-asst 'x))             ; not 1 argument
    (check-error (find-formula-true-asst 'x (lambda () 'fail)))   ; not 2 args
    (check-error
       (find-formula-true-asst 'x (lambda () 'fail) (lambda (c r) 'succeed) z)) ; not 4 args

    (check-error (find-formula-true-asst 'x (lambda () 'fail) (lambda () 'succeed)))
        ; success continuation expects 2 arguments, not 0
    (check-error (find-formula-true-asst 'x (lambda () 'fail) (lambda (_) 'succeed)))
        ; success continuation expects 2 arguments, not 1
    (check-error (find-formula-true-asst '(and x (not x)) (lambda (_) 'fail) (lambda (_) 'succeed)))
        ; failure continuation expects 0 arguments, not 1


    (check-expect   ; x can be solved
       (find-formula-true-asst 'x
                               (lambda () 'fail)
                               (lambda (cur resume) 'succeed))
       'succeed)

    (check-expect   ; x is solved by '((x #t))
       (find-formula-true-asst 'x
                               (lambda () 'fail)
                               (lambda (cur resume) (find 'x cur)))
       '#t)

    (check-expect   ; (not x) can be solved
       (find-formula-true-asst '(not x)
                               (lambda () 'fail)
                               (lambda (cur resume) 'succeed))
       'succeed)

    (check-expect   ; (not x) is solved by '((x #f))
       (find-formula-true-asst '(not x)
                               (lambda () 'fail)
                               (lambda (cur resume) (find 'x cur)))
       '#f)

    (check-expect   ; (and x (not x)) cannot be solved
       (find-formula-true-asst '(and x (not x))
                               (lambda () 'fail)
                               (lambda (cur resume) 'succeed))
       'fail)

