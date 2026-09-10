use v6;
use MONKEY-SEE-NO-EVAL;
use Test;

# A write whose TARGET NAME is resolved at run time — `$::($name) = v`,
# `::('$x') = v`, or an assignment inside an `EVAL`'d snippet — must reach the
# outer lexical it names, at mainline, inside a bare block, inside an invoked
# closure, inside a routine, and inside a block that a Raku-level routine calls.
# The compiler cannot see such a target, so every compile-time filter in the
# frame-exit writeback used to drop it and the write was silently lost.
#
# Every assertion here also passes under real `raku` (that is what makes it a
# specification pin rather than a mutsu-behaviour pin).

plan 28;

# --- $::(...) symbolic deref --------------------------------------------------

{
    my $z = 1;
    $::('z') = 11;
    is $z, 11, 'symbolic-deref write at mainline';
}

{
    my $z = 1;
    { $::('z') = 22 }
    is $z, 22, 'symbolic-deref write inside a bare block';
}

{
    my $z = 1;
    my $c = { $::('z') = 33 };
    $c();
    is $z, 33, 'symbolic-deref write inside an invoked closure';
}

{
    my $z = 1;
    sub sd-call(&f) { f() }
    sd-call({ $::('z') = 44 });
    is $z, 44, 'symbolic-deref write inside a closure passed to a sub';
}

{
    my $z = 1;
    sub sd-body() { $::('z') = 55 }
    sd-body();
    is $z, 55, "symbolic-deref write inside a named sub's own body";
}

{
    my $z = 1;
    sub sd-nested(&f) { my $inner = { f() }; $inner() }
    sd-nested({ $::('z') = 66 });
    is $z, 66, 'symbolic-deref write two frames deep';
}

{
    my $z = 1;
    lives-ok { $::('z') = 77 }, 'symbolic-deref write inside a lives-ok block lives';
    is $z, 77, '... and the write reached the outer lexical';
}

{
    my @a = <a b c>;
    my $c = { @::('a') = ('X', 'Y', 'Z') };
    $c();
    is @a.join(' '), 'X Y Z', 'symbolic-deref @-write inside an invoked closure';
}

# --- ::('$x') indirect lookup -------------------------------------------------

{
    my $z = 1;
    my $c = { ::('$z') = 88 };
    $c();
    is $z, 88, 'indirect-lookup write inside an invoked closure';
}

{
    my $z = 1;
    sub il-body() { ::('$z') = 99 }
    il-body();
    is $z, 99, "indirect-lookup write inside a named sub's own body";
}

# --- EVAL ---------------------------------------------------------------------

{
    my $a;
    EVAL q|$a = 32|;
    is $a, 32, 'EVAL write to an outer lexical at mainline';
}

{
    my $a;
    { EVAL q|$a = 32| }
    is $a, 32, 'EVAL write to an outer lexical inside a bare block';
}

{
    my $a;
    my $c = { EVAL q|$a = 32| };
    $c();
    is $a, 32, 'EVAL write to an outer lexical inside an invoked closure';
}

{
    my $a;
    sub ev-call(&c) { &c() }
    ev-call({ EVAL q|$a = 32| });
    is $a, 32, 'EVAL write inside a closure passed to a sub';
}

{
    my $a;
    sub ev-body() { EVAL q|$a = 32| }
    ev-body();
    is $a, 32, "EVAL write inside a named sub's own body";
}

{
    my $a = 7;
    my $c = { EVAL q|$a = 32| };
    $c();
    is $a, 32, 'EVAL write to an already-initialized outer lexical inside a closure';
}

{
    my $a;
    lives-ok { EVAL q|$a = ' 32 '| }, 'EVAL write inside a lives-ok block lives';
    is $a, ' 32 ', '... and the write reached the outer lexical';
}

{
    my $a;
    my $c = { EVAL q|$a = 1|; EVAL q|$a = $a + 41| };
    $c();
    is $a, 42, 'a second EVAL in the same block still sees the outer lexical';
}

# An `is rw` writeback performed by a routine the EVAL'd snippet calls must reach
# the caller's lexicals the same way (roast S06-signature/sigilless.t).
{
    sub rw-swap(\x, \y) { my $t = y; y = x; x = $t }
    my $p = 5;
    my $q = 3;
    my $c = { EVAL q|rw-swap($p, $q)| };
    $c();
    is "$p|$q", '3|5', 'sigilless rw writeback through an EVAL inside a closure';
}

{
    sub rw-swap2(\x, \y) { my $t = y; y = x; x = $t }
    my $p = 5;
    my $q = 3;
    lives-ok { EVAL q|rw-swap2($p, $q)| }, 'sigilless rw EVAL inside lives-ok lives';
    is "$p|$q", '3|5', '... and the swap reached the caller lexicals';
}

# --- must NOT regress: a `my` inside the EVAL stays EVAL-scoped ----------------

{
    my $a = 1;
    EVAL q|my $a = 999|;
    is $a, 1, "EVAL's own `my` does not clobber a same-named caller lexical";
}

{
    my $a = 1;
    sub ev-my() { EVAL q|my $a = 999| }
    ev-my();
    is $a, 1, "EVAL's own `my` stays EVAL-scoped inside a routine too";
}

{
    my $a = 1;
    my $c = { EVAL q|my $a = 999| };
    $c();
    is $a, 1, "EVAL's own `my` stays EVAL-scoped inside a closure too";
}

{
    # A brand-new lexical the snippet declares must not leak into the caller.
    EVAL q|my $eval-scoped-only = 5|;
    nok EVAL(q|$*eval-leak-probe|).defined,
        'a `my` declared only inside an EVAL does not become visible outside';
}

# --- must NOT regress: an unrelated same-named lexical is untouched -----------

{
    my $outer = 'kept';
    sub shadow-writer() { my $outer = 'inner'; $::('outer') = $outer }
    shadow-writer();
    ok $outer.defined, 'a runtime-name write does not undefine an outer lexical';
}

# vim: expandtab shiftwidth=4
