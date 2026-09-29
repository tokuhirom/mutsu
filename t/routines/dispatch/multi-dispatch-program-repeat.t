use Test;

# A value-dependent multi is specialized per argument-type key after its first
# call (#10107); every later call with the same key runs the specialized
# program. These call each shape several times so both the first (building)
# call and the later (program) calls are covered, and pin that they agree.

plan 9;

# Two equally narrow constrained candidates that both match: the first
# declared wins, on every call.
multi twin(Int $x where * > 0) { 'first' }
multi twin(Int $x where * > 0) { 'second' }
is (^3).map({ twin(5) }).join(','), 'first,first,first', 'equal-rank constrained candidates';

# `is default` settles a tie between identical nominal candidates.
multi dflt(Int $x)            { 'plain' }
multi dflt(Int $x) is default { 'default' }
is (^3).map({ dflt(1) }).join(','), 'default,default,default', 'is default breaks the tie';

# The winner changes with the value under one type key.
subset Small of Int where * < 100;
multi size(Small $x)                 { 'small' }
multi size(Int $x where * < 10_000)  { 'medium' }
multi size(Int $x)                   { 'large' }
is (5, 5000, 50_000, 7, 7000).map({ size($_) }).join(','),
    'small,medium,large,small,medium', 'values alternate under one key';

# A different type key builds its own program.
multi size(Str $x where *.chars < 3) { 'short' }
multi size(Str $x)                   { 'long' }
is ('ab', 'abcd', 'x').map({ size($_) }).join(','), 'short,long,short', 'a second key';
is size(5), 'small', 'the first key is unaffected';

# A type object and an instance key apart.
multi tobj(Int:U $x) { 'type' }
multi tobj(Int:D $x where * > 0) { 'positive' }
multi tobj(Int:D $x) { 'other' }
is (Int, 3, -3, Int).map({ tobj($_) }).join(','), 'type,positive,other,type', 'definedness';

# No candidate matches: the same failure every time.
multi only-pos(Int $x where * > 0) { 'pos' }
for ^2 {
    throws-like { only-pos(-1) }, X::Multi::NoMatch, 'no match raises (call ' ~ $_ ~ ')';
}

# A predicate runs exactly once per call.
my $runs = 0;
multi counted(Int $x where { $runs++; $_ > 0 }) { 'counted' }
multi counted(Int $x) { 'fallback' }
counted(1) for ^4;
is $runs, 4, 'one predicate run per call';

done-testing;
