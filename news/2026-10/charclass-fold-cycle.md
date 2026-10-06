# Self-referential regex character class no longer overflows the stack

A grammar rule whose class assertion named the rule itself (`regex name { <-restricted +name -sep> }`) re-entered the parse-time token class fold without end and crashed with SIGSEGV. The fold now tracks the tokens it is folding and declines a re-entrant attempt, so the call raises a catchable exception as in Rakudo.
