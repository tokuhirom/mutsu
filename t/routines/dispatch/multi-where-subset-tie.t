use v6;
use Test;

# A `where` clause and a `subset` type are the same kind of refinement for
# multi dispatch: each is a bind-time check on top of a nominal type. Rakudo
# ranks a constrained parameter above an unconstrained one of the same nominal
# type, and draws no line between the two kinds of constraint, so two
# candidates that differ only in which kind they use tie and declaration order
# decides. mutsu used to rank every `where` above every subset.

plan 7;

subset Small of Int where * < 100;

multi s1(Small $x)                { "subset" }
multi s1(Int $x where * < 10_000) { "where" }
multi s1(Int $x)                  { "plain" }
is s1(5), "subset", 'subset declared first wins the tie with a where';
is s1(500), "where", 'the where candidate when the subset rejects';
is s1(50_000), "plain", 'the unconstrained candidate when both reject';

multi s2(Int $x where * < 10_000) { "where" }
multi s2(Small $x)                { "subset" }
is s2(5), "where", 'where declared first wins the tie with a subset';

multi s3(Small $x, Int $y)   { "one" }
multi s3(Small $x, Small $y) { "two" }
is s3(5, 5), "two", 'more constrained parameters is narrower';

multi s4(Int $x) { "plain" }
multi s4(Small $x) { "subset" }
is s4(5), "subset", 'a subset still beats its unconstrained base type';

multi s5(Int $x where * > 0) { "where" }
multi s5(Small $x where * > 0) { "both" }
is s5(5), "where", 'a parameter with a subset and a where counts once';
