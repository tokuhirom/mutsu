use Test;

plan 12;

# A `state` declaration is usually not the whole expression it sits in:
# `++state $n` parses as a prefix operator wrapped around the declaration, and
# `@r.push(state $n // 0)` buries it in a call argument. The compiler decides
# whether an inline nested block (an `if` branch, a bare block) needs a
# `ResetStateLocals` by asking whether its statements declare `state` at their
# own level — and that test only recognized a declaration that WAS the whole
# expression. So `if 1 { ++state $n }` emitted no reset and its `state` kept
# counting across calls, where raku restarts it with the branch's clone.

sub incr()  { my @r; if 1 { @r.push(++state $n) }; @r.join(',') }
is (incr(), incr(), incr()).join('|'), "1|1|1", 'prefix ++ around a state decl';

sub post()  { my @r; if 1 { (state $n)++; @r.push($n) }; @r.join(',') }
is (post(), post(), post()).join('|'), "1|1|1", '...and a postfix ++';

sub bin()   { my @r; if 1 { @r.push((state $n = 0) + 1); $n++ }; @r.join(',') }
is (bin(), bin(), bin()).join('|'), "1|1|1", '...an operand of an infix';

sub arg()   { my @r; if 1 { @r.push(++(state $n)) }; @r.join(',') }
is (arg(), arg(), arg()).join('|'), "1|1|1", '...inside a call argument';

sub tern($c) { my @r; if 1 { @r.push($c ?? ++state $n !! 0) }; @r.join(',') }
is (tern(1), tern(1)).join('|'), "1|1", '...a ternary branch';

sub arr()   { my @r; if 1 { @r.push([++state $n].join('')) }; @r.join(',') }
is (arr(), arr()).join('|'), "1|1", '...an array literal element';

sub meth()  { my @r; if 1 { @r.push((++state $n).Str) }; @r.join(',') }
is (meth(), meth()).join('|'), "1|1", '...a method-call invocant';

sub interp() { my @r; if 1 { @r.push("{++state $n}") }; @r.join(',') }
is (interp(), interp()).join('|'), "1|1", '...an interpolated string';

sub bare()  { my @r; { @r.push(++state $n) }; @r.join(',') }
is (bare(), bare()).join('|'), "1|1", 'a bare block resets it too';

# The same walk decides whether a `state` initializer's evaluation can be
# skipped once initialized: a nested `state` in the RHS must still run its own
# StateVarInit on every call.
sub nested() { state $a = (state $b = 0) + 1; $b++; "$a/$b" }
is (nested(), nested(), nested()).join('|'), "1/1|1/2|1/3",
    'a nested state decl in an initializer keeps its own init';

# What must NOT restart: a `state` at the routine's own level, however it is
# wrapped, belongs to the routine's clone.
sub plain() { ++state $n }
is (plain(), plain(), plain()).join(','), "1,2,3",
    'a wrapped state at routine level keeps counting';

sub modif() { my @r; @r.push(++state $n) if 1; @r.join(',') }
is (modif(), modif(), modif()).join('|'), "1|2|3",
    'a postfix if introduces no block, so it must not reset';
