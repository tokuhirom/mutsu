use Test;

# A nominally typed readonly parameter is bound straight to its argument, so
# `$x =:= Int` holds for the type object; an untyped, `is copy` or `is rw`
# parameter owns a Scalar (#16959).

plan 12;

sub t1(Int $x) { $x =:= Int }
sub t2(Int :$x) { $x =:= Int }
sub t3(Int $x?) { $x =:= Int }
sub t4(Int $x = Int) { $x =:= Int }
sub t5(Int $x = 5) { $x =:= 5 }
sub u1($x) { $x =:= Int }
sub u2($x is copy) { $x =:= Int }
sub u3(Int $x is copy) { $x =:= Int }
sub u4(Any $x) { $x =:= Int }
sub v1(\x) { x =:= Int }
sub w1(Int $x) { $x =:= 5 }
sub w2($x) { $x =:= 5 }

is t1(Int), True, 'positional typed parameter';
is t2(:x(Int)), True, 'named typed parameter, supplied';
is t2(), True, 'named typed parameter, unsupplied';
is t3(), True, 'optional typed parameter, unsupplied';
is t4(), True, 'typed parameter defaulting to the type object';
is t5(), True, 'typed parameter defaulting to a literal';
is u1(Int), False, 'untyped parameter owns a Scalar';
is u2(Int), False, 'is copy owns a Scalar';
is u3(Int), False, 'typed is copy owns a Scalar';
is v1(Int), True, 'sigilless parameter';
is w1(5), True, 'typed parameter holding a literal';
is w2(5), False, 'untyped parameter holding a literal';
