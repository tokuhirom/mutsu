use Test;
plan 5;

# sink - evaluates and discards result
{
    sink say("hello");
    pass 'sink evaluates its expression';
}

# o - function composition
{
    my &double = * * 2;
    my &inc = * + 1;
    my &comp = &inc o &double;
    is comp(3), 7, 'o composes functions (inc(double(3)) = 7)';
    is comp(5), 11, 'o composes functions (inc(double(5)) = 11)';
}

# Multiple composition
{
    my &triple = * * 3;
    my &double = * * 2;
    my &inc = * + 1;
    my &comp = &inc o &double o &triple;
    is comp(2), 13, 'chained o composition (inc(double(triple(2))) = 13)';
}

# Unicode ∘
{
    my &double = * * 2;
    my &inc = * + 1;
    my &comp = &inc ∘ &double;
    is comp(4), 9, '∘ Unicode compose operator';
}

done-testing;
