use v6;
use Test;

# A sigilless declaration (`my \name := expr` / `my \name = expr`) binds the
# name to `expr` rather than assigning into a container, since a sigilless
# term has none. When `expr` is itself a plain variable reference, raku makes
# the sigilless name a writable ALIAS of that variable's underlying
# container — assigning through the alias writes through to the original
# variable. When `expr` is any other rvalue (a literal, a computed value),
# the sigilless name stays genuinely readonly.
#
# mutsu previously blanket-marked every untyped sigilless bind as readonly
# (ignoring what it was bound to) and, conversely, blanket-marked every
# TYPED sigilless bind as writable but WITHOUT actually aliasing the source
# container — so `my Int \x := $x; x = 10` silently mutated only the local
# `x`, leaving `$x` unchanged. See todo/tickets/dist-test-suite-failures-batch.md
# (Math::Interval triage) for how this was found.

# --- := bind to a plain variable: writes through to the source ---
{
    my $x = 5;
    my \x := $x;
    x = 10;
    is $x, 10, ':= bind to $var writes through on assignment via the alias';
}

{
    my $x = 5;
    my \x := $x;
    $x = 20;
    is x, 20, ':= bind to $var reflects a write to the source variable too';
}

# --- := bind to a literal/computed rvalue: stays readonly ---
{
    my \x := 5;
    dies-ok { x = 10 }, ':= bind to a literal stays readonly';
}

{
    dies-ok { my \x := 5 + 3; x = 10 }, ':= bind to a computed rvalue stays readonly';
}

# --- typed sigilless bind: mutability follows the RHS shape too, not the
#     presence of a type constraint ---
{
    my Int $x = 5;
    my Int \x := $x;
    x = 10;
    is $x, 10, 'typed := bind to $var truly aliases the source (writes through)';
}

{
    dies-ok { my Int \x := 5; x = 10 }, 'typed := bind to a literal still stays readonly';
}

# --- plain `=` on a sigilless decl behaves like `:=` (no container to
#     assign into, so it binds too) ---
{
    my $a = 5;
    my \x = $a;
    x = 10;
    is $a, 10, '= form bind to $var also writes through (raku: no assignment target exists)';
}

{
    dies-ok { my \x = 5; x = 10 }, '= form bind to a literal stays readonly';
}

# --- multiple independent sigilless binds in the same scope ---
{
    my ($a, $b) = (1, 2);
    my \x := $a;
    my \y := $b;
    x = 100;
    y = 200;
    is $a, 100, 'first of two independent := binds writes through';
    is $b, 200, 'second of two independent := binds writes through';
}

# --- reading through the alias reflects the current value ---
{
    my $x = 1;
    my \x := $x;
    $x++;
    $x++;
    is x, 3, 'reading the sigilless alias reflects mutation of the source var';
}

# --- sigilless bind still works for common idioms not touching write-through ---
{
    my \pi = 3.14159;
    is pi, 3.14159, 'sigilless constant-style decl reads back correctly';
}

{
    sub double(\n) { n * 2 }
    is double(21), 42, 'sigilless sub parameter still works (unrelated path, no regression)';
}

{
    my @a = (1, 2, 3);
    my \first = @a[0];
    is first, 1, 'sigilless bind to an array element read (not a bare $var) stays readonly-safe';
}

done-testing;
