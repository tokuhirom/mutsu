use Test;

# An extended name's colonpair value is canonicalized by its compile-time
# value, so `:k[1,2]`, `:k(1,2)` and `:k<1 2>` all name `k<1 2>` -- with a
# subscript-adverb name as the key too (#18029).
#
# Kept apart from adverbial-pair-variable-name.t: a `[1,2]` / `(1,2)` value
# in a declaration cannot be spelled back by the RakuAST `.AST` view yet
# (#18612), so this file is not on ci/rakuast-ast-eval-passing.txt.

plan 6;

{
    my $y:k[1,2] = 3;
    is $y:k<1 2>, 3, ':k with a [] list value names k<1 2>';
    is $y:k[1,2], 3, '... read back with the same [] spelling';
    is $y:k(1,2), 3, '... and with ()';
}

{
    my $z:a(1,2) = 4;
    is $z:a<1 2>, 4, ':a with a () list value names a<1 2>';
    is $z:a[1,2], 4, '... read back with []';
}

{
    my $w:v[1] = 5;
    is $w:v<1>, 5, 'a one-item [] list';
}
