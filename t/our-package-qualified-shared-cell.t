use v6;
use Test;

# `our $x` and its package-qualified name (`$GLOBAL::x` at file scope) must
# name the SAME container: a write through either name must be visible
# through the other. See todo/tickets/our-var-and-its-package-name-are-two-slots.md.

# Mainline `our` + a package-qualified `=` write.
{
    our $a = 1;
    $GLOBAL::a = 5;
    is $a, 5, 'mainline our $a sees a $GLOBAL::a = ... write';
    is $GLOBAL::a, 5, '$GLOBAL::a itself reads back the new value';
}

# Mainline `our` + a package-qualified `++`.
{
    our $b = 1;
    $GLOBAL::b++;
    is $b, 2, 'mainline our $b sees a $GLOBAL::b++ write';
}

# Mainline `our` + a package-qualified `+=`.
{
    our $c = 1;
    $GLOBAL::c += 1;
    is $c, 2, 'mainline our $c sees a $GLOBAL::c += 1 write';
}

# A plain lexical assignment through the bare `our` name must still reach
# the package var (this direction already worked before the fix — pin it as
# a non-regression).
{
    our $d = 1;
    $d = 9;
    is $d, 9, 'plain $d = 9 updates the our-declared lexical';
    is $GLOBAL::d, 9, '... and is visible through $GLOBAL::d too';
}

# A package var modified from a DIFFERENT compilation unit (EVAL) must be
# visible through the enclosing `our` declaration's lexical alias — the
# exact shape roast/S02-names/our.t test 10 (RT69460) exercises.
{
    our $rt69460 = 1;
    lives-ok { EVAL 'class RT69460EVAL { $GLOBAL::rt69460++ }' },
        'can compile a class that modifies our variable via EVAL';
    is $rt69460, 2, 'class compiled by EVAL can modify the our variable';
}

done-testing;
