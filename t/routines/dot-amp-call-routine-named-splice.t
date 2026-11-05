use Test;

# String::Splice's t/01-basic.rakutest: `(^10).map: { $s.=flip.=&splice($s.chars/2, "$_") }`
# with String::Splice's own `proto sub splice`. `$x.&f(args)` compiles as the direct call
# `f($x, args)`, but for the list-operator names the parser keeps apart when a program
# declares one (`splice`, `push`, `say`, ...) that direct call lost the distinction: inside
# a `map` block it resolved the core list routine and died "Routine does not have any
# candidates. Is only the proto defined?".

plan 9;

proto sub splice(Str(Any) $, |c) is pure {*};
multi splice(Str(Any) $str, Int(Real) $start, Str $add = '' --> Str) { "$str|$start|$add" }

my $s = 'ab';
is $s.&splice(1, "X"), 'ab|1|X', 'a plain .&splice';
is (^1).map({ $s.&splice(1, "X") }).list[0], 'ab|1|X', '.&splice inside a map block';
is (^1).map({ $s.&splice(1/2, "X") }).list[0], 'ab|0|X', 'with a Rat start (Int(Real) coerces)';
is (^1).grep({ $s.&splice(1, "X") }).elems, 1, 'inside a grep block';
my $t = 'ab';
is (^1).map({ $t.=flip.=&splice(1, "$_") }).list[0], 'ba|1|0', 'the chained .= forms in a map block';

# The core routines of the same names keep working through .&name.
my @a = 1, 2, 3;
(^1).map({ @a.&push(4) });
is @a.join(','), '1,2,3,4', '.&push on an array in a map block';
is (^1).map({ 5.&say }).elems, 1, '.&say in a map block';

# A user routine named like another list method.
sub shift(Str $x) { "mine-shift $x" }
my $u = 'cd';
is (^1).map({ $u.&shift }).list[0], 'mine-shift cd', 'a user sub shift through .&shift in a map block';
is $u.&shift, 'mine-shift cd', 'and outside one';
