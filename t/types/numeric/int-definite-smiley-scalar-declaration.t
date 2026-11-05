use Test;

plan 11;

# A definite native type on a scalar declaration refuses the boxed value
# assigned to it (rakudo's RakuAST frontend); a parameter of that type binds
# the value unboxed (#16707).
for 'my int:D $n = 3; $n', 'my num:D $n = 3e0; $n', 'my str:D $s = "a"; $s' -> $code {
    my $died = False;
    my $msg = '';
    try { EVAL $code; CATCH { default { $died = True; $msg = .message } } }
    ok $died, "$code dies";
    like $msg, /'Type check failed in assignment to'/, "... as an assignment type check";
}

throws-like { EVAL 'my int:D $n; 1' }, X::Syntax::Variable::MissingInitializer,
    'no initializer for a definite native';

is (EVAL 'my Int:D $n = 3; $n'), 3, 'a boxed definite type still takes it';
is (EVAL 'my int $n = 3; $n'), 3, 'a plain native still takes it';
is (EVAL 'sub f(int:D $x) { $x }; f(2)'), 2, 'a native definite parameter binds the value';
is (EVAL 'my int:_ $n = 3; $n'), 3, 'a :_ native takes it';
