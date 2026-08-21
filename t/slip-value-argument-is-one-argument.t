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

# --- method-call and hyper-method-call forms (ADR-0054 Slice 3) ---
class C { method m($a) { $a.elems } }
my $c = C.new;
my $mname = 'm';

# Bareword target (non-variable) -- compiles to CallMethod.
is C.m(maybe(0)), 0, 'method call (CallMethod): Slip result of a non-firing if is one argument';

# Variable target -- compiles to CallMethodMut.
is $c.m(maybe(0)), 0, 'method call (CallMethodMut): Slip result of a non-firing if is one argument';

# Dynamic method name, bareword target -- compiles to CallMethodDynamic.
is C."$mname"(maybe(0)), 0,
    'dynamic method call (CallMethodDynamic): Slip result of a non-firing if is one argument';

# Dynamic method name, variable target -- compiles to CallMethodDynamicMut.
is $c."$mname"(maybe(0)), 0,
    'dynamic method call (CallMethodDynamicMut): Slip result of a non-firing if is one argument';

class D { method n($a) { "n:" ~ $a.raku } }
my @objs = D.new, D.new;

# Hyper method call, static name -- compiles to HyperMethodCall.
is-deeply (@objs>>.n(maybe(0))).List, ("n:Empty", "n:Empty").List,
    'hyper method call (HyperMethodCall): Slip result of a non-firing if is one argument per element';

# Hyper method call, dynamic name -- compiles to HyperMethodCallDynamic.
my $dyn_mname = 'n';
is-deeply (@objs>>."$dyn_mname"(maybe(0))).List, ("n:Empty", "n:Empty").List,
    'hyper dynamic method call (HyperMethodCallDynamic): Slip result of a non-firing if is one argument per element';

# --- `|EXPR` still spreads correctly for method/hyper forms too ---
class G { method g2($a, $b) { "$a-$b" } }
my $g = G.new;
my @s2 = (1, 2);
is $g.g2(|@s2), '1-2', 'method call: |@array spreads into a fixed 2-arity method';
is G.g2(|@s2), '1-2', 'method call (bareword target): |@array spreads into a fixed 2-arity method';

class K { method k(*@a) { @a.elems } }
my $k = K.new;
is $k.k(|@s2), 2, 'method call: |@array spreads into a slurpy method';
is $k.k(@s2.Slip), 2, 'method call: @array.Slip flattens into a slurpy method (slurpy binding is call-site independent)';

done-testing;
