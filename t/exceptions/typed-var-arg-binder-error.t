use Test;

plan 5;

# A variable declared with a SUPERTYPE of its parameter's type is not refuted at
# compile time by rakudo: the binder reports the run-time failure, not "Calling
# f(Str) will never work" (#19071).

sub f(Int $x) { }

sub supertype-var { my Cool $s = "a"; f($s) }
throws-like { supertype-var() }, X::TypeCheck::Binding::Parameter,
    'a Cool variable passed to an Int parameter fails in the binder';

sub param-forwards(Cool $c) { f($c) }
throws-like { param-forwards("b") }, X::TypeCheck::Binding::Parameter,
    'a Cool-typed parameter forwarded to an Int parameter fails in the binder';

throws-like { EVAL q[sub g(Int $x) { }; my Cool $t = "a"; g($t)] },
    X::TypeCheck::Binding::Parameter,
    'the same inside EVAL';

# A variable whose declared type is unrelated is still refuted statically.
throws-like { EVAL q[sub g(Int $x) { }; my Str $t = "a"; g($t)] },
    X::TypeCheck::Argument,
    'an unrelated declared type is still refuted at compile time';

sub ok-var { my Int $i = 3; f($i); 'ok' }
is ok-var(), 'ok', 'a variable of the parameter\'s own type binds';
