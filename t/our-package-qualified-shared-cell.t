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

# `our $x` REDECLARED with no initializer (`our $x;` after `our $x = <val>`)
# reads back the current value and must NOT store the raw ContainerRef cell
# back into itself: doing so makes the cell hold a `ContainerRef` pointing at
# itself, and any LATER read/write of it locks its own Mutex twice on the
# same thread and hangs forever. See roast/S04-declarations/our.t tests
# 28-29 (`is our $foo, 3, ...`), caught via a CI timeout while landing the
# shared-cell fix.
{
    sub bar() { our $foo = 3 };
    is bar(), 3, 'return value of sub call declaring our-scoped var';
    is our $foo, 3, 'bare our redeclaration (expression position) reads back the value';
    is $foo, 3, '... and the value stays';
    $GLOBAL::foo = 42;
    is $foo, 42, '... and the cell still write-throughs correctly afterward';
}

done-testing;
