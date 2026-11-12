use Test;

# With user `infix:<ne>` / `infix:<!=>` candidates in scope, a negated
# comparison against a Junction is still the negation of the positive
# comparison over the whole Junction, as rakudo's core `ne` (`not a eq b`)
# takes it: `$x ne any(@r)` means "equal to none of @r" (#17513: Red's
# table-removal loop `@pending.grep({ $_ ne any(@ready) })` never shrank).

multi infix:<ne>(Int $a, Int $b) { 'user ne' }
multi infix:<!=>(Str $a, Str $b) { 'user !=' }

plan 8;

nok 'a' ne any(<a b>), 'ne any: equal to one of them';
ok 'c' ne any(<a b>), 'ne any: equal to none';
ok 'a' ne all(<b c>), 'ne all';
nok 1 != any(1, 2), '!= any: equal to one of them';
ok 3 != any(1, 2), '!= any: equal to none';
is-deeply <a b c d>.grep({ $_ ne any(<b d>) }), <a c>, 'a grep that removes several values';
is (3 ne 4), 'user ne', 'the user ne candidate still takes the calls it accepts';
is ('a' != 'b'), 'user !=', 'and so does the user != candidate';
