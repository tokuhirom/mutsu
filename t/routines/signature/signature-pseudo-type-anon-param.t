use v6;
use Test;

# Regression (#7954, the `expected statement ...` parse-failure index): a
# parameter whose whole text is the pseudo-type `::?CLASS` / `::?ROLE`, with no
# variable and no `:` marker after it. Only the `:` marker declares an invocant,
# so such a parameter is an ANONYMOUS POSITIONAL exactly as `sub f(Int)` is. The
# parser read every non-variable continuation as an invocant instead, which made
# `multi prefix:<-->(::?CLASS) is export { ... }` (CRDT) a hard
# X::Syntax::Signature::InvocantNotAllowed — a `sub` may not take an invocant —
# and quietly turned `method m(::?CLASS)` into a zero-argument method.

plan 14;

class Plain {
    sub takes-one(::?CLASS) { 'sub' }
    multi sub multi-takes-one(::?CLASS) { 'multi sub' }
    method via-sub { takes-one(self) }
    method via-multi-sub { multi-takes-one(self) }

    method one-arg(::?CLASS) { 'method' }
    multi method dispatched(::?CLASS) { 'object' }
    multi method dispatched(Int) { 'int' }
    method smiley(::?CLASS:D) { 'definite' }
}

# A `sub` nested in the class: an anonymous positional, not a rejected invocant.
is Plain.new.via-sub, 'sub', '`sub f(::?CLASS)` takes one positional argument';
is Plain.new.via-multi-sub, 'multi sub', 'and so does the `multi sub` form';

# A method: the parameter is a positional, so it is in ADDITION to the invocant.
is Plain.new.one-arg(Plain.new), 'method', '`method m(::?CLASS)` takes one positional argument';
is Plain.new.one-arg(Plain), 'method', 'and it accepts the type object too';

# It is a real type constraint, not a name-only placeholder.
dies-ok { Plain.new.one-arg(3) }, 'the pseudo-type still rejects an argument of another type';

# Multi dispatch sees it as a real candidate parameter.
is Plain.new.dispatched(Plain.new), 'object', 'the pseudo-type candidate matches an instance';
is Plain.new.dispatched(3), 'int', 'and the sibling candidate still matches an Int';

# A type smiley on the anonymous positional.
is Plain.new.smiley(Plain.new), 'definite', '`::?CLASS:D` works as an anonymous positional';
dies-ok { Plain.new.smiley(Plain) }, 'and its `:D` rejects the type object';

# The `:` invocant marker keeps its meaning in every spelling.
class Marked {
    method attached(::?CLASS: $x) { $x }
    method smiley-attached(::?CLASS:D: $x) { $x }
    method spaced(::?CLASS:D : $x) { $x }
}
is Marked.new.attached(7), 7, '`::?CLASS: $x` still declares the invocant';
is Marked.new.smiley-attached(7), 7, 'and `::?CLASS:D: $x` does too';
is Marked.new.spaced(7), 7, 'and a whitespace-separated marker does too';

role Marker {
    method bare(::?ROLE:) { 'role invocant' }
}
class DoesMarker does Marker { }
is DoesMarker.new.bare, 'role invocant', '`::?ROLE:` still declares the invocant';

# A named parameter written after the pseudo-type is still a named parameter,
# not an invocant (the `:` of `:from(:$for)` is the named marker).
class Named {
    method create(::?CLASS:D :from(:$for)) { $for.WHAT.^name }
}
is Named.new.create(:for(Named.new)), 'Named', 'a named parameter after the pseudo-type is still named';
