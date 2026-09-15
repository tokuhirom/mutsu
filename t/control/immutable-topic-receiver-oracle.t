use Test;

# The compile-time "this receiver yields bare items" oracle
# (`Compiler::for_iterable_yields_bare_items`) decides whether a `for` loop's
# implicit topic, or a `.map`/`.grep`/`.first` callback's `$_`, is assignable.
# raku binds the topic to the source ELEMENT, so `$_ = ...` is legal exactly
# when that element has a `Scalar` behind it.
#
# Every row below was verified against rakudo; the file is designed to pass
# under `raku` too, so a wrong expectation here fails against the oracle.

plan 61;

# --- a `%` variable mints a fresh Pair per entry: the topic is that Pair,
#     which is not in a Scalar, so `$_ = ...` is refused ------------------
{
    my %h = a => 1, b => 2;
    dies-ok { for %h { $_ = 5 } }, 'for %h { $_ = ... } dies';
    is %h<a>, 1, 'for %h { $_ = ... } left the hash alone';
}
{
    my %h = a => 1, b => 2;
    dies-ok { %h.map({ $_ = 5 }).eager }, '%h.map({ $_ = ... }) dies';
}
{
    my %h = a => 1, b => 2;
    dies-ok { %h.grep({ $_ = 5 }).eager }, '%h.grep({ $_ = ... }) dies';
}

# --- but the Pair OBJECT stays mutable: the marking is SHALLOW ----------
{
    my %h = a => 1, b => 2;
    for %h { .value = 5 }
    is-deeply %h, {a => 5, b => 5}, 'for %h { .value = ... } still writes through';
}
{
    my %h = a => 1, b => 2;
    for %h -> $p { $p.value = 7 }
    is-deeply %h, {a => 7, b => 7}, 'for %h -> $p { $p.value = ... } still writes through';
}
{
    my %h = a => 1;
    for %h.pairs { .value = 9 }
    is-deeply %h, {a => 9}, 'for %h.pairs { .value = ... } still writes through';
}
{
    my %h = a => 1, b => 2;
    my $n = 0;
    for %h { $n += .value }
    is $n, 3, 'for %h reads .value normally';
}

# --- coercers/views that mint fresh items -------------------------------
{
    my @a = 1, 2;
    dies-ok { for @a.List { $_ = 5 } }, 'for @a.List { $_ = ... } dies';
    is-deeply @a, [1, 2], '.List left the array alone';
}
{
    my @a = 1, 2;
    dies-ok { @a.List.map({ $_ = 5 }).eager }, '@a.List.map({ $_ = ... }) dies';
}
{
    my @a = 1, 2;
    dies-ok { @a.List.grep({ $_ = 5 }).eager }, '@a.List.grep({ $_ = ... }) dies';
}
{
    my @a = 1, 2;
    dies-ok { for @a.pairs { $_ = 5 } }, 'for @a.pairs { $_ = ... } dies';
}
{
    my @a = 1, 2;
    dies-ok { for @a.antipairs { $_ = 5 } }, 'for @a.antipairs { $_ = ... } dies';
}
{
    my %h = a => 1;
    dies-ok { for %h.kv { $_ = 5 } }, 'for %h.kv { $_ = ... } dies';
    is %h<a>, 1, '%h.kv left the hash alone';
}
{
    my %h = a => 1;
    dies-ok { for %h.List { $_ = 5 } }, 'for %h.List { $_ = ... } dies';
}

# --- .pairs still hands out writable values ----------------------------
{
    my @a = 1, 2;
    for @a.pairs { .value = 5 }
    is-deeply @a, [5, 5], 'for @a.pairs { .value = ... } still writes through';
}

# --- a LIST literal is element-wise: an item with no Scalar behind it
#     makes the topic immutable ------------------------------------------
{
    my @a = 1, 2;
    dies-ok { for (@a,) { $_ = 5 } }, 'for (@a,) { $_ = ... } dies';
    is-deeply @a, [1, 2], '(@a,) left the array alone';
}
{
    my @a = 1, 2;
    my @b = 3, 4;
    dies-ok { for @a, @b { $_ = 5 } }, 'for @a, @b { $_ = ... } dies';
}
{
    my $x = 1;
    my @a = 2, 3;
    dies-ok { for $x, @a { $_ = 9 } }, 'for $x, @a { $_ = ... } dies';
}
{
    my $x = 1;
    dies-ok { for $x, 2 { $_ = 5 } }, 'for $x, 2 { $_ = ... } dies';
}
{
    my $x = 1;
    dies-ok { for ($x + 1,) { $_ = 5 } }, 'for ($x + 1,) { $_ = ... } dies';
}
{
    dies-ok { for (1..2), (3..4) { $_ = 5 } }, 'for (1..2), (3..4) { $_ = ... } dies';
}
{
    my @a = 1, 2;
    dies-ok { (@a,).map({ $_ = 5 }).eager }, '(@a,).map({ $_ = ... }) dies';
}
{
    my %h = a => 1;
    dies-ok { for (%h,) { $_ = 5 } }, 'for (%h,) { $_ = ... } dies';
}

# --- CONTROL: a list literal made only of Scalar-denoting items keeps a
#     writable topic ------------------------------------------------------
{
    my $x = 1;
    my $y = 2;
    for ($x, $y) { $_ = 5 }
    is "$x $y", "5 5", 'for ($x, $y) { $_ = ... } still writes through';
}
{
    my $x = 1;
    my $y = 2;
    ($x, $y).map({ $_ = 5 }).eager;
    is "$x $y", "5 5", '($x, $y).map({ $_ = ... }) still writes through';
}
{
    my $x = 1;
    for ($x,) { $_ = 5 }
    is $x, 5, 'for ($x,) { $_ = ... } still writes through';
}

# --- CONTROL: `.list` and `.values` iterate the source's own containers -
{
    my @a = 1, 2;
    for @a.list { $_ = 5 }
    is-deeply @a, [5, 5], 'for @a.list { $_ = ... } still writes through';
}
{
    my @a = 1, 2;
    for @a.values { $_ = 5 }
    is-deeply @a, [5, 5], 'for @a.values { $_ = ... } still writes through';
}
{
    my %h = a => 1;
    for %h.values { $_ = 5 }
    is-deeply %h, {a => 5}, 'for %h.values { $_ = ... } still writes through';
}
{
    my @a = 1, 2;
    for @a { $_ = 5 }
    is-deeply @a, [5, 5], 'for @a { $_ = ... } still writes through';
}
{
    my @a = 1, 2;
    for @a.Seq { $_ = 5 }
    is-deeply @a, [5, 5], 'for @a.Seq { $_ = ... } still writes through';
}

# --- `.first` joins `.map`/`.grep` in consulting the same oracle --------
{
    dies-ok { (1, 2).first({ $_ = 5 }) }, '(1, 2).first({ $_ = ... }) dies';
}
{
    my @a = 1, 2;
    dies-ok { @a.List.first({ $_ = 5 }) }, '@a.List.first({ $_ = ... }) dies';
}
# CONTROL: `.first` over a real array still scans the element containers.
{
    my @a = 1, 2;
    is @a.first({ $_ = 5 }), 5, '@a.first({ $_ = ... }) answers the written value';
    is-deeply @a, [5, 2], '@a.first({ $_ = ... }) still writes through';
}
{
    my @a = 1, 2;
    @a.values.first({ $_ = 5 });
    is-deeply @a, [5, 2], '@a.values.first({ $_ = ... }) still writes through';
}
{
    my @a = 3, 4, 5;
    is @a.first({ $_ > 3 }), 4, '.first with an ordinary matcher is unaffected';
}
{
    my @a = 3, 4, 5;
    is @a.first(* > 3), 4, '.first with a Whatever matcher is unaffected';
}

# --- the same oracle gates ADR-0045's `is rw` / `<->` BIND: a source that can
#     only yield bare values has no container to alias, and raku fails the bind
#     before the body runs ---------------------------------------------------
{
    my %h = a => 1;
    dies-ok { for %h <-> $p { } }, 'for %h <-> $p fails the bind';
}
{
    my @a = 1, 2;
    dies-ok { for @a.List <-> $v { } }, 'for @a.List <-> $v fails the bind';
}
{
    my @a = 1, 2;
    dies-ok { for @a.pairs <-> $p { } }, 'for @a.pairs <-> $p fails the bind';
}
{
    my @a = 1, 2;
    dies-ok { for (@a,) <-> $v { } }, 'for (@a,) <-> $v fails the bind';
}
{
    my %h = a => 1;
    dies-ok { for %h.kv <-> $v { } }, 'for %h.kv <-> $v fails the bind';
}
{
    my %h = a => 1;
    dies-ok { for %h -> $p is rw { } }, 'for %h -> $p is rw fails the bind';
}
# CONTROL: an all-Scalar list literal still binds and writes back.
{
    my $x = 1;
    my $y = 2;
    for $x, $y <-> $v { $v = 9 }
    is "$x $y", "9 9", 'for $x, $y <-> $v still binds and writes back';
}
# CONTROL: a read-only named param over the same sources still binds.
{
    my %h = a => 1;
    my @k;
    for %h -> $p { @k.push: $p.key }
    is-deeply @k, ["a"], 'for %h -> $p (read-only) still binds';
}
{
    my @a = 1, 2;
    my $n;
    for (@a,) -> $v { $n = $v.elems }
    is $n, 2, 'for (@a,) -> $v (read-only) still binds';
}

# --- issue #7556 section A(rest): a receiver the oracle cannot decide from
#     its own syntax, because it is a bare variable rather than a literal or
#     a coercer chain written directly against one. Closing these needs a
#     compile-time fact about what the VARIABLE denotes, tracked from its own
#     declaration (`Compiler::provably_bare_receiver_vars`) rather than from
#     the receiver expression alone. -------------------------------------

# `.Seq` written directly (no intermediate variable): it reifies whatever
# items its target already has, so it inherits the target's bareness exactly.
{
    dies-ok { (1, 2, 3).Seq.map({ $_ = 5 }).eager },
        '(1,2,3).Seq.map({ $_ = ... }) dies';
}
{
    dies-ok { (1, 2, 3).Seq.grep({ $_ = 5 }).eager },
        '(1,2,3).Seq.grep({ $_ = ... }) dies';
}
# CONTROL: `.Seq` over a real array's own elements still writes through
# (pinned above too, at line 172, for a `for` loop) — confirming the new
# `.Seq` arm does not regress the case it must stay silent on.
{
    my @a = 1, 2;
    @a.Seq.map({ $_ = 5 }).eager;
    is-deeply @a, [5, 5], '@a.Seq.map({ $_ = ... }) still writes through';
}

# An `@`-variable `:=`-bound to a List literal: the bind itself denotes an
# immutable Positional, and nothing in `@a.map(...)`'s own syntax says so.
#
# The declaration must live INSIDE the `dies-ok` block along with its use: a
# block passed as a call argument (like `dies-ok`'s) compiles as a genuine
# closure with its own fresh `Compiler`
# (`Compiler::push_dynamic_scope_lexical`'s doc), which does not inherit the
# calling `Compiler`'s `provably_bare_receiver_vars` — the same reason
# `local_types` cannot see an outer scope's type constraint from inside a
# nested closure either. Declaring `@a`/`$s` outside and using it only inside
# would defeat the tracking, not exercise it.
{
    dies-ok { my @a := (1, 2, 3); @a.map({ $_ = 5 }).eager },
        ':=-bound @a.map({ $_ = ... }) dies';
}
{
    dies-ok { my @a := (1, 2, 3); @a.grep({ $_ = 5 }).eager },
        ':=-bound @a.grep({ $_ = ... }) dies';
}
{
    dies-ok { my @a := (1, 2, 3); for @a { $_ = 5 } },
        'for a :=-bound @a { $_ = ... } dies';
}

# A `$`-variable holding a `.Seq`: same shape, scalar sigil.
{
    dies-ok { my $s = (1, 2, 3).Seq; $s.map({ $_ = 5 }).eager },
        '$s (holding a Seq).map({ $_ = ... }) dies';
}
{
    dies-ok { my $s = (1, 2, 3).Seq; $s.grep({ $_ = 5 }).eager },
        '$s (holding a Seq).grep({ $_ = ... }) dies';
}

# CONTROL: the compile-time fact must not survive a REBIND/reassignment to
# something that is not provably bare — that direction is a false positive
# (a spurious throw raku does not have), never acceptable for this oracle.
{
    my @a := (1, 2, 3);
    my @m = (9, 9, 9);
    @a := @m;
    @a.map({ $_ = 5 }).eager;
    is-deeply @m, [5, 5, 5], 'rebinding away from a bare List drops the mark';
}
{
    my $s = (1, 2, 3).Seq;
    $s = [7, 8, 9];
    $s.map({ $_ = 5 }).eager;
    is-deeply $s, [5, 5, 5], 'reassigning away from a Seq drops the mark';
}
