use Test;

# A package stash's `$` entry (`GLOBAL::<$G>`, `Foo::<$n>`) is the package
# variable: `++`/`--` and `+=` on it create or update `$GLOBAL::G`, and a
# later stash read of the same key sees what the assignment wrote, for an
# uppercase-leading name too (#18127).

plan 11;

GLOBAL::<$G> = 1;
is GLOBAL::<$G>, 1, 'a stash read sees the stash assignment';
is $GLOBAL::G, 1, '... as does the qualified variable';
GLOBAL::<$G> = GLOBAL::<$G> + 1;
is $GLOBAL::G, 2, 'a read-modify-write through the stash';
GLOBAL::<$G> += 1;
is $GLOBAL::G, 3, '+= on the stash entry';

GLOBAL::<$H>++;
is $GLOBAL::H, 1, '++ creates the package variable';
GLOBAL::<$H>++;
is $GLOBAL::H, 2, '... and increments it again';
GLOBAL::<$H>--;
is GLOBAL::<$H>, 1, '-- decrements it';

package Foo { our $n = 1 }
Foo::<$n>++;
is $Foo::n, 2, "++ on a named package's existing variable";
Foo::<$m>++;
is $Foo::m, 1, "... and on one it does not have yet";

++GLOBAL::<$P>;
is $GLOBAL::P, 1, 'prefix ++';
is GLOBAL::<$Q>++, 0, 'postfix ++ returns the old value';
