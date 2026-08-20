use Test;

# ADR-0054: argument-list interpolation is a call-site property, decided by
# `|EXPR` syntax, never by a value's runtime Slip-shape. `t/slip-arg-flatten.t`
# already pins this rule for the statement-call path and for callees with a
# slurpy/list parameter; every case there binds into a slurpy, so it cannot
# see a FIXED-arity callee mis-flatten. This file covers exactly that gap:
# a routine whose tail conditional does not fire evaluates to `Empty` (a
# `Slip`), and passing that result along as a plain (non-`|`) argument to a
# fixed-arity routine must stay ONE argument, across the function, listop,
# code-variable and method call forms (all dual-oracle verified against
# `raku` first; see docs/adr/0054-argument-list-interpolation-is-a-call-site-property.md).

sub maybe($x) { if $x { 42 } }

# --- the dominant real-world shape: a non-firing tail `if` ---

sub show($a) { $a.elems }

# Function-call form (`f(...)`) -- compiles to CallFunc.
is show(maybe(0)), 0, 'function call: Slip result of a non-firing if is one argument';

# Listop-style form (no parens) -- a plain user sub still compiles through
# the same Expr::Call path as the parenthesized form (Stmt::Call/ExecCall is
# reserved for a narrow builtin/imported-function allowlist), so this is a
# second observation of the same fix rather than a different mechanism --
# kept as its own case because the ADR's motivating table lists it
# separately.
{
    my $r = show maybe(0);
    is $r, 0, 'listop-style call: Slip result of a non-firing if is one argument';
}

# Code-variable form (`&c(...)`) -- compiles to CallOnCodeVar.
{
    my &c = &show;
    is c(maybe(0)), 0, 'code-variable call: Slip result of a non-firing if is one argument';
}

# --- the full §2.2 matrix for a fixed-arity callee ---

sub g($a) { $a.elems }

is g(Empty), 0, 'g(Empty): the empty Slip is one argument';
{
    my $r = g Empty;
    is $r, 0, 'g Empty (listop-style): the empty Slip is one argument';
}
is g(().Slip), 0, 'g(().Slip): an ordinary Slip-valued argument is one argument';

my @s = (1, 2);
is g(@s.Slip), 2, 'g(@s.Slip): a non-empty Slip-valued argument stays one argument ($a.elems == 2)';

my &gc = &g;
is gc(Empty), 0, 'code-variable call: g(Empty) is one argument';
is gc(@s.Slip), 2, 'code-variable call: g(@s.Slip) stays one argument';

# --- `|EXPR` still spreads correctly (the OTHER half of the same rule) ---

sub g2($a, $b) { "$a-$b" }
is g2(|(1, 2)), '1-2', '|(...) spreads into a fixed 2-arity callee';
is g2(|@s), '1-2', '|@array spreads into a fixed 2-arity callee';

sub k(*@a) { @a.elems }
is k(|@s), 2, '|@array spreads into a slurpy callee';
is k(@s.Slip), 2, '@array.Slip flattens into a slurpy callee (slurpy binding is call-site independent, §2.3)';

# --- method-call form: not yet fixed (ADR-0054 Slice 3) ---
class C { method m($a) { $a.elems } }
{
    todo 'method dispatch does not yet spread by call-site syntax -- ADR-0054 Slice 3';
    lives-ok { C.m(maybe(0)) }, 'method call: Slip result of a non-firing if is one argument';
}

done-testing;
