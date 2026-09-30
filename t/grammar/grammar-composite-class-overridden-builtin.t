use Test;

plan 6;

# A named item in a composite class is a call to the rule of that name, so a
# grammar's own `alpha` / `digit` replaces the built-in class.
grammar G { token alpha { 'Z' }; token TOP { <+alpha -[q]>+ } }
ok !G.parse("aZb").defined, 'overridden alpha rejects built-in letters';
is G.parse("ZZ").Str, "ZZ", 'overridden alpha accepts its own token';

grammar D { token digit { 'x' }; token TOP { <+digit>+ } }
is D.parse("x").Str, "x", 'overridden digit accepts its own token';
ok !D.parse("1").defined, 'overridden digit rejects built-in digits';

# Without an override the built-in still applies.
grammar B { token TOP { <+alpha +digit>+ } }
is B.parse("a1b2").Str, "a1b2", 'no override: built-ins apply';

# A negated overridden item excludes only what its token matches.
grammar N { token digit { 'x' }; token TOP { <-digit>+ } }
is N.parse("1a").Str, "1a", 'negated overridden digit ignores built-in digits';

done-testing;
