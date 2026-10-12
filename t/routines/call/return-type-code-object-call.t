use Test;

# A named routine called through its code object (`&f(...)`, or a closure's
# call of a block-declared routine, ADR-12529 phase 2) checks its return type
# exactly as a call by name does. Checked against rakudo.

plan 4;

{
    my sub ret-t(::T \t, $v --> T) { $v }
    is &ret-t(Int, 42), 42, 'a return type naming a type capture resolves in the callee';
    my $c = { ret-t(Int, 43) };
    is $c(), 43, 'the same from a closure nested in the declaring block';
}

{
    my sub make(--> Code:D) { EVAL q[sub inner($x) { $x }] }
    my &t = &make();
    is t(6), 6, 'a sub an EVAL declares does not inherit the caller\'s return type';
    is-deeply &t.returns, Mu, 'its .returns is its own';
}
