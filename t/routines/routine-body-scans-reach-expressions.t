use Test;

# Routine- and block-level facts the compiler reads off a body must see
# expression positions too (ADR-0137 ports of the body scans).

plan 4;

# A statement modifier opens no block: `state $n = 0 if 1` declares in the
# `if` block's own scope, which raku re-clones on every call of the sub.
{
    my @seen;
    sub f { if 1 { state $n = 0 if 1; @seen.push(++$n) } }
    f() for ^3;
    is-deeply @seen, [1, 1, 1], 'state behind an if modifier restarts with its block';
}

# `return-rw` as an operand still hands the caller the container.
{
    my $x = 1;
    sub g { 1 and return-rw $x }
    g() = 5;
    is $x, 5, 'return-rw behind `and` returns the container';
}

# An expression-position `return` with an argument is rejected by a
# definite return value, like a statement-level one.
{
    throws-like 'sub h(--> 42) { 1 and return 5 }; h()', Exception,
        message => /'No return arguments allowed'/,
        'expression-position return with an argument vs a definite return value';
    lives-ok { EVAL 'sub k(--> 42) { 1 and return; }; k()' },
        'expression-position bare return is allowed';
}
