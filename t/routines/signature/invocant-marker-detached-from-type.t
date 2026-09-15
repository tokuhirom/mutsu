use Test;

# The invocant marker may be written apart from the type it marks. `method
# m(C:D :)` and `method m(C : $y)` declare exactly the same anonymous typed
# invocant as the glued `method m(C:D:)`, but only the glued spelling parsed,
# so a signature with a gap failed at its closing paren and took the whole
# enclosing file down with it (Tree::Binary::Role::BinaryTree writes
# `method iterator (Tree::Binary::Role::BinaryTree:D :)`).
#
# The gap must not swallow a typed NAMED parameter: `sub f(Int :$x)` binds
# `$x`, and the invocant marker never binds the thing to its right.
#
# This pins rakudo's behaviour, not mutsu's: every assertion below is green
# under rakudo itself.

plan 9;

class C {
    method glued(C:D:)        { 'glued' }
    method gap-smiley(C:D :)  { 'gap-smiley' }
    method gap-plain(C :)     { 'gap-plain' }
    method gap-args(C:D : $y) { $y }
    method gap-plain-args(C : $y, $z) { $y + $z }
}

is C.new.glued, 'glued', 'the glued anonymous typed invocant still parses';
is C.new.gap-smiley, 'gap-smiley', 'a gap before the marker keeps the :D invocant';
is C.new.gap-plain, 'gap-plain', 'a gap before the marker keeps the plain-type invocant';
is C.new.gap-args(7), 7, 'a detached marker still separates the invocant from the parameters';
is C.new.gap-plain-args(2, 3), 5, 'and from several of them';

# A typed named parameter is not an invocant, gap or no gap.
sub named(Int :$x) { $x }
is named(x => 4), 4, 'a typed named parameter still binds its argument';

sub named-many(Str $a, Int :$b) { "$a/$b" }
is named-many('s', b => 1), 's/1', 'a typed named parameter after a positional still binds';

# A named invocant with a gap keeps working.
class D {
    method named-invocant($x :) { 'named-invocant' }
}
is D.new.named-invocant, 'named-invocant', 'a named invocant with a gap still parses';

# The marker really does make it an invocant: the method takes no arguments.
is C.new.gap-smiley.chars, 10, 'the marked parameter is the invocant, not a positional';
