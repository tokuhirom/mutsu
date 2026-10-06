use Test;

# `Num.Rat(eps)`, `Rat.Rat(eps)` and the `.FatRat(eps)` forms bind a `Real
# $epsilon`: a Str, Nil, a type object or a Complex fails the bind with
# X::TypeCheck::Binding::Parameter instead of being silently replaced by the
# default 1e-6 (#12043). An `Int` invocant is already rational and never binds
# the epsilon, so `7.Rat('0.01')` still answers. Every expected answer is
# Rakudo 2026.09's.

plan 36;

my $binding = X::TypeCheck::Binding::Parameter;

# --- a non-Real epsilon fails the bind
throws-like { 3.7e0.Rat('0.01') }, $binding,
    message => /"parameter 'epsilon'" .* 'expected Real but got Str'/,
    'Num.Rat(Str)';
throws-like { 3.7.Rat('0.01') }, $binding,
    message => /"parameter '<anon>'" .* 'expected Real but got Str'/,
    'Rat.Rat(Str)';
throws-like { 3.7e0.FatRat('0.01') }, $binding,
    message => /"parameter '\$epsilon'" .* 'expected Real but got Str'/,
    'Num.FatRat(Str)';
throws-like { 3.7.FatRat('x') }, $binding,
    message => /"parameter '<anon>'" .* 'expected Real but got Str'/,
    'Rat.FatRat(Str)';
throws-like { 3.7e0.Rat(Any) }, $binding,
    message => /'expected Real but got Any (Any)'/, 'Num.Rat(Any)';
throws-like { 3.7e0.Rat(Nil) }, $binding,
    message => /'expected Real but got Nil (Nil)'/, 'Num.Rat(Nil)';
throws-like { 3.7e0.Rat(1+0i) }, $binding,
    message => /'expected Real but got Complex (<1+0i>)'/, 'Num.Rat(Complex)';
throws-like { 3.7e0.FatRat(Any) }, $binding, 'Num.FatRat(Any)';
throws-like { 3.7.Rat(Nil) }, $binding, 'Rat.Rat(Nil)';
throws-like { (3.7+0i).Rat('0.01') }, $binding, 'Complex.Rat(Str)';
throws-like { (3.7+0i).FatRat('0.01') }, $binding, 'Complex.FatRat(Str)';

# --- the failing epsilon is only checked where it is bound
is 7.Rat('0.01'), 7.0, 'Int.Rat ignores a Str epsilon';
is 7.Rat('0.01').raku, '7.0', '... and answers a Rat';
is 7.FatRat('0.01').raku, 'FatRat.new(7, 1)', 'Int.FatRat ignores a Str epsilon';

# --- every kind of Real is an epsilon
is 3.14159e0.Rat(0.01).raku, '<22/7>', 'a Rat epsilon';
is 3.14159e0.Rat(1e-2).raku, '<22/7>', 'a Num epsilon';
is 3.7e0.Rat(1), 3.0, 'an Int epsilon';
is 3.7e0.Rat(True), 3.0, 'a Bool epsilon is 1';
is 3.7e0.Rat(False).raku, '3.7', 'False is a zero epsilon';
is 3.7e0.Rat(<1>), 3.0, 'an IntStr epsilon';
is 3.7e0.Rat(<1.0>), 3.0, 'a RatStr epsilon';
is 3.7e0.Rat(<1e0>), 3.0, 'a NumStr epsilon';
is 3.7e0.Rat(<0.01>), 3.7, 'a RatStr epsilon below the precision';
is 3.7e0.Rat(1.0.FatRat), 3.0, 'a FatRat epsilon';
is 3.7e0.Rat(10**30), 3.0, 'an Int epsilon too big for 64 bits';
is 3.7e0.Rat(Duration.new(0.01)), 3.7, 'a Duration is a Real';

# --- the FatRat forms accept the same Reals
is 3.14159e0.FatRat(0.01).raku, 'FatRat.new(22, 7)', 'Num.FatRat(Rat)';
is 3.7e0.FatRat(True).raku, 'FatRat.new(3, 1)', 'Num.FatRat(Bool)';
is 3.7e0.FatRat(<1>).raku, 'FatRat.new(3, 1)', 'Num.FatRat(IntStr)';
is 3.7.FatRat(<0.01>).raku, 'FatRat.new(37, 10)', 'Rat.FatRat(RatStr)';

# --- the default epsilon is unchanged
is 3.7e0.Rat.raku, '3.7', 'the zero-argument form';
is 3.7e0.Rat(0).raku, '3.7', 'a zero epsilon';
is (3.14159+0i).Rat(0.01).raku, '<22/7>', 'Complex.Rat(Rat) forwards the epsilon';
is (3.14159+0i).FatRat(0.01).raku, 'FatRat.new(22, 7)', 'Complex.FatRat(Rat) forwards the epsilon';
is (3.7+0i).Rat(True), 3.0, 'Complex.Rat(Bool)';
is 3.7e0.Rat(0.01).^name, 'Rat', 'the answer is a Rat';
