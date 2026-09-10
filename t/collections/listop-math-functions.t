use Test;
plan 14;

# Math functions should work as listops (consume full expression as argument)
is-approx exp(1 + 2), exp(3), 'exp with addition in parens';
is exp(1 + 2), exp(3), 'exp 1+2 equals exp(3)';

# Listop behavior: exp without parens consumes the expression
{
    my $r = exp 1 + 2;
    is-approx $r, exp(3), 'exp as listop consumes full expression';
}

{
    my $r = sqrt 4 + 5;
    is-approx $r, sqrt(9), 'sqrt as listop consumes full expression';
}

{
    my $r = log 1 + 0;
    is-approx $r, log(1), 'log as listop consumes full expression';
}

{
    my $r = sin 0 + 0;
    is-approx $r, sin(0), 'sin as listop consumes full expression';
}

{
    my $r = cos 0 + 0;
    is-approx $r, cos(0), 'cos as listop consumes full expression';
}

# abs already worked as listop, verify still works
is abs(-5 + 3), abs(-2), 'abs with parens';

# postfix:<i> can be called as a function
is postfix:<i>(42), 0+42i, 'postfix:<i> as function call';
is postfix:<i>(3.14), 0+3.14i, 'postfix:<i> with Num as function call';

# Named math functions with complex arguments
is-approx exp(i * 3.141592653589793), -1+0i, 'exp(i * pi) is approximately -1';
is-approx Num(exp(i * 3.141592653589793)), -1, 'Num(exp(i * pi)) is approximately -1';

# floor/ceiling/round/truncate as listops
{
    my $r = floor 3.7 + 0.1;
    is $r, floor(3.8), 'floor as listop';
}

# exp with two args (base)
is exp(3, 3), 27, 'exp with base argument';
