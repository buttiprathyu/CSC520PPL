; Template for SAT Solver Test Cases 

(record not [arg])   ;; OK if these are duplicates
(record or  [args])
(record and [args])


; This case is important because the formula cannot
; be solved hence there is no solution
(val f1 '(and (or x (not y)) (and (not x) y)))
(val s1 'no-solution)

; This case is important because it tests and, or, not operators' nested
; functionality. The formula is solved if (x #f) and (y #f)
; and doesn't depend on the value of z
(val f2 '(and (not (or x (and y z)))
              (or (not (and x y (or z x))) (and (not x) y z))))
(val s2 '((x #f) (y #f)))

; This case is important because the formula is satisfied
; only when x and y evaluate to false that is when the formula is true
(val f3 '(not (or x (and y (not x)))))
(val s3 '((x #f) (y #f)))
