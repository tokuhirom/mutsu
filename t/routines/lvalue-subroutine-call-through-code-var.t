use Test;

# #10965: an `is rw` routine returned from EVAL and stored in an outer
# `&`-variable is assigned through the code object the call site names,
# never re-resolved by its declared name (lexical to the EVAL unit).

plan 8;

{
    my $v = 1;
    my &k = EVAL Q[sub h($x is rw) is rw { $x }; &h];
    k($v) = 5;
    is $v, 5, 'assign through a renamed EVAL-returned is rw sub';
}

{
    my $v = 1;
    my &h = EVAL Q[sub h($x is rw) is rw { $x }; &h];
    h($v) = 7;
    is $v, 7, 'assign through an EVAL-returned is rw sub stored under its own name';
}

{
    my $v = 1;
    my &k = EVAL Q[sub h($x is raw) is raw { $x }; &h];
    k($v) = 9;
    is $v, 9, 'is raw routine from EVAL writes through too';
}

{
    my $v = 1;
    my &k = EVAL Q[sub h($x is rw) { return-rw $x }; &h];
    k($v) = 11;
    is $v, 11, 'return-rw routine from EVAL writes through';
}

{
    my $v = 1;
    my &k = EVAL Q[sub h($x is rw) is rw { $x }; &h];
    k($v)++;
    is $v, 2, 'postfix ++ through an EVAL-returned is rw sub';
}

{
    my $v = 1;
    my &k = sub h($x is rw) is rw { $x };
    ++k($v);
    k($v)--;
    k($v)++;
    is $v, 2, '++/-- through an is rw sub held in a lexical &-variable';
}

{
    my &k = EVAL Q[sub h($x) { 42 }; &h];
    throws-like { k(1) = 5 }, X::Assignment::RO,
        'a non-rw EVAL-returned sub is still refused';
}

{
    sub h($x is rw) is rw { $x }
    my $v = 1;
    my &k = EVAL Q[sub h($x) { 42 }; &h];
    throws-like { k($v) = 5 }, X::Assignment::RO,
        'the call site\'s code object wins over an outer routine of the same name';
}
