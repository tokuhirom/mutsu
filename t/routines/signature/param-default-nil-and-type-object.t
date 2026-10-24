use Test;

# A parameter default of `Nil` or a type object is a constant, bound without
# evaluating the default (#14812). The binder must keep producing exactly what
# evaluating it would: the value itself, an explicit argument winning, the
# declared type still checked, and later defaults still seeing it.

plan 21;

sub sa($x = Nil) { $x.raku }
sub sb(Int $x = Int) { $x.raku }
sub sc($x = Any) { $x.raku }
sub sf($x = Nil, $y = $x) { "{$x.raku} {$y.raku}" }
sub sg(Mu $x = Mu) { $x.raku }
sub sh(Cool $x = Str) { $x.raku }
sub sj(Numeric $x = Int) { $x.raku }
sub sk($x = Nil) { $x.defined }
sub sl(Any $x = Nil) { $x.raku }

is sa, 'Nil', 'Nil default';
is sa(5), '5', 'an argument wins over a Nil default';
is sb, 'Int', 'Int default for an Int parameter';
is sb(3), '3', 'an argument wins over a type-object default';
is sc, 'Any', 'Any default';
is sc(1), '1', 'an argument wins over the Any default';
is sf, 'Nil Nil', 'a later default reads the earlier constant';
is sg, 'Mu', 'Mu default for a Mu parameter';
is sh, 'Str', 'Str default for a Cool parameter';
is sj, 'Int', 'Int default for a Numeric parameter';
is sk, False, 'the Nil default is not defined';
is sl, 'Nil', 'Nil default for an Any parameter';

multi mm($x = Nil) { "any {$x.raku}" }
is mm, 'any Nil', 'a multi candidate with a Nil default';

class K { method mm($x = Any) { $x.raku } }
is K.new.mm, 'Any', 'a method with a type-object default';
is K.new.mm(7), '7', 'and an argument to it';

# The default is a value, not a shared container.
sub keep($x = Nil) { my $y = $x; $y = 5; $x }
is keep, Nil, 'assigning a copy does not change the default';
is keep, Nil, 'and the next call still sees Nil';

# A constraint the default does not satisfy is still the general binder's call.
sub typed(Int $x = 5) { $x }
is typed, 5, 'a literal default for a typed parameter';
dies-ok { EVAL 'sub bad(Int $x = "no") { $x }; bad()' }, 'a default of the wrong type is rejected';

# Calls in a loop (the light call path) agree with the first call.
my @seen;
@seen.push(sa) for ^3;
is-deeply @seen, ['Nil', 'Nil', 'Nil'], 'repeated calls agree';
@seen = ();
@seen.push(sc(Int)) for ^2;
is-deeply @seen, ['Int', 'Int'], 'a type-object argument wins over the default';
