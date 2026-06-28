use Test;

# A `my`/`our` declaration that carries a conditional statement modifier keeps
# its *declaration* lexically unconditional (declarations take effect at compile
# time in Raku); only the *initializer* is gated by the modifier. Regression for
# roast S04-declarations/my-6e.t ('my $x if 0').

plan 12;

# Bare declaration with a false modifier: variable is still declared (Any).
{
    my $a;
    my $x if 0;
    lives-ok { $a = $x }, 'my $x if 0 still declares $x (no init to gate)';
    is $a.^name, 'Any', '$x reads as Any after `my $x if 0`';
}

# Initializer gated off: variable declared but undefined.
{
    my $x = 5 if 0;
    nok $x.defined, 'my $x = 5 if 0 leaves $x undefined';
}

# Initializer gated on: variable declared and initialized.
{
    my $x = 5 if 1;
    is $x, 5, 'my $x = 5 if 1 initializes $x';
}

# `unless` modifier (inverted condition).
{
    my $x = 5 unless 1;
    nok $x.defined, 'my $x = 5 unless 1 leaves $x undefined';

    my $y = 9 unless 0;
    is $y, 9, 'my $y = 9 unless 0 initializes $y';
}

# Array / hash defaults when the init is gated off.
{
    my @a = 1, 2, 3 if 0;
    is @a.elems, 0, 'my @a = ... if 0 leaves an empty array';

    my %h = a => 1 if 0;
    is %h.elems, 0, 'my %h = ... if 0 leaves an empty hash';
}

# Typed declaration with gated-off init keeps the declared type object.
{
    my Int $x = 5 if 0;
    is $x.^name, 'Int', 'my Int $x = 5 if 0 keeps the Int type object';
}

# The declared variable is visible to sibling statements in the same scope.
{
    my $x = 10 if 1;
    my $sum = $x + 1;
    is $sum, 11, 'conditionally-declared $x is visible to later statements';
}

# `state` is NOT split — its once-only init semantics are preserved.
{
    sub counter { state $n = 0 if 1; $n++; $n }
    is (counter(), counter(), counter()), (1, 2, 3),
        'state $n = 0 if 1 keeps once-only state semantics';
}

# A normal postfix-if on a non-declaration statement is unaffected.
{
    my $hit = 0;
    $hit = 1 if True;
    is $hit, 1, 'plain postfix-if statement modifier still works';
}
