# A nullary term keyword (`self`, `now`, `time`, `pi`, ...) is a complete term,
# so what follows it is an OPERATOR position -- rakudo's `term:sym<self>` never
# takes a parenthesized argument list. mutsu used to read the `(` after one as
# the start of a no-paren listop call's first argument, so the set-difference
# infix `(-)` became `self((-) %allowed)` and died with
# "Unknown prefix operator: (-)".
#
# From Hash::Restricted 0.0.9 (`t/01-basic.rakutest`), whose restricting roles
# compare the hash against the allowed key set with `self (-) %allowed` inside
# their own `STORE`.

use Test;

plan 8;

my $allowed = set('a');

# The construct the distribution actually uses, inside a method.
class SetDiffProbe {
    method diff-from-self() { self (-) $allowed }
}
ok SetDiffProbe.new.diff-from-self ~~ Set,
  'self (-) $set parses as set-difference inside a method';

# Each of these is a term in rakudo, so the `(-)` after it is the infix.
ok (Any (-) $allowed) ~~ Set, 'Any (-) $set is set-difference';
ok (now (-) $allowed) ~~ Set, 'now (-) $set is set-difference';
ok (time (-) $allowed) ~~ Set, 'time (-) $set is set-difference';
ok (pi (-) $allowed) ~~ Set, 'pi (-) $set is set-difference';

# The other bracketed set infixes go the same way.
is (pi (|) $allowed).elems, 2, 'pi (|) $set is set-union';
# `$allowed` holds "a" and `pi` is not a member, so the intersection is empty.
is (pi (&) $allowed).elems, 0, 'pi (&) $set is set-intersection';

# A term keyword with an ADJACENT paren is still rakudo's "Undeclared routine":
# only the spaced form is an operator position, so the call form stays rejected.
eval-dies-ok 'now(1)', 'now(...) is not a call';
