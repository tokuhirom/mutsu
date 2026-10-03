use Test;

plan 11;

# A Seq is neither a List nor Positional; it binds to an `@` parameter
# through PositionalBindFailover instead.
nok (1, 2).Seq ~~ List, 'a Seq is not a List';
nok (1, 2).Seq ~~ Positional, 'a Seq is not Positional';
ok (1, 2).Seq ~~ PositionalBindFailover, 'a Seq is a PositionalBindFailover';

# Multi dispatch still lets an untyped `@` parameter take a Seq.
multi pick-one(@p) { 'positional' }
multi pick-one(Mix $m) { 'mix' }
is pick-one((1, 2).Seq), 'positional', 'multi @ candidate accepts a Seq';
is pick-one((1, 2).map(* + 1)), 'positional', 'multi @ candidate accepts a map Seq';
is pick-one(3.Mix), 'mix', 'a Mix still picks the Mix candidate';

multi named-pos(:@a) { @a.elems }
is named-pos(a => (1, 2, 3).Seq), 3, 'named @ parameter accepts a Seq';

# A `--> List(Seq)` return coerces the Seq it returns.
sub gen(UInt $n --> List(Seq)) { 1 xx $n }
is gen(3).^name, 'List', '--> List(Seq) coerces the returned Seq to a List';

# An `is copy` @ parameter bound to a Seq is a mutable Array.
sub flip-first(@c is copy) { @c[0] = !@c[0]; @c }
is-deeply flip-first(True xx 2), [False, True], 'is copy @ param copies a Seq into an Array';

# A `Positional`-typed parameter binds a Seq through the failover too
# (rakudo#4864), in a plain sub and in multi dispatch.
sub typed-pos(Positional $p) { $p.^name }
is typed-pos((1, 2).Seq), 'List', 'Positional-typed parameter accepts a Seq as a List';
multi typed-multi(Positional $p) { 'positional' }
multi typed-multi($x) { 'any' }
is typed-multi((1, 2).Seq), 'positional', 'multi Positional candidate wins for a Seq';
