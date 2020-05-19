val constraints =
 [ TYVAR "a" ~ TYCON "sym" /\ TYVAR "b" ~ TYCON "bool"
 , CONAPP (TYCON "pair", [TYVAR "a", TYVAR "b"]) ~ TYCON "int"
 , TYCON "sym" ~ TYCON "int"
 ]