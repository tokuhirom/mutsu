# .^methods lists the methods a built-in class declares itself

`Map.^methods`, `Pair.^methods`, `Set.^methods` and the other catalog classes whose
introspectable triage was incomplete now also list the names their catalog rows declare
(the class's own method table), for every class of the MRO below `Cool` (#13494).
`Map` went from 5 listed methods to 37, `Pair` from 2 to 22; the rest of the gap to
Rakudo is catalog rows that are not yet baked (`Pair.freeze`, `Pair.iterator`, ...).
