# Declares an `infix:<*>` of its own (so the operator is in scope here), but
# never imports OpScope::Where's candidate.
unit module OpScope::OwnOp;
multi infix:<*>(Str $a, Str $b) is export(:str) { "$a$b" }
sub own-mul($a, $b) is export { $a * $b }
