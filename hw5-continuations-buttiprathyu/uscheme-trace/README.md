# uscheme-trace

Code written by Norman Ramsey.
Put into a private github repository for CSc 520 Spring 2020 by Michelle Strout.
This code should NOT be shared with anyone not taking CSc 520.

To create the uscheme interpreter that can trace function calls
type 'make'.

// example usage
./uscheme-trace
-> (val &trace 50)
50
-> (car (cdr '((a b) (c d))))
(cdr ((a b) (c d))) => ...
(cdr ((a b) (c d))) => ((c d))
(car ((c d))) => ...
(car ((c d))) => (c d)
(c d)


Page 218 in the Ramsey book talks more about how the trace facility works.

