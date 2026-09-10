use Test;

# `proto`/`multi sub` declarations are lexically scoped, exactly like a plain
# `sub`. mutsu's routine registry is keyed by the fully-qualified
# `Package::name`, so an inner scope's `proto` collided with an outer one and
# was rejected outright with "Redeclaration of routine 'foo'" -- valid Raku
# refused at declaration time. Every expectation below was measured against
# Rakudo v2026.06 (2026-09-04) first; raku is the oracle.
#
# The inner scopes below are statement-form bare blocks. The value-position
# `do { ... }` form scopes its routine declarations too (it did not until
# news/2026-09/do-block-scopes-its-routine-declarations.md); that form has its
# own pin file, t/do-block-scopes-routine-decls.t.

plan 22;

# --- 1. plain single sub shadowing (was already correct; guard it) ----------
{
    sub s1() { "outer" }
    my $inner;
    { sub s1() { "inner" }; $inner = s1(); }
    is $inner, "inner", "plain sub shadows in an inner block";
    is s1(), "outer", "the outer plain sub comes back after the block";
}

# --- 2. proto+multi shadowed in an inner block ------------------------------
{
    proto sub s2($) {*}
    multi sub s2(Int $x) { "outer" }
    my $inner;
    {
        proto sub s2($) {*}
        multi sub s2(Int $x) { "inner" }
        $inner = s2(5);
    }
    is $inner, "inner", "an inner proto+multi shadows the outer one";
    is s2(5), "outer", "the outer proto+multi comes back after the block";
}

# --- 3. the same, inside a routine body instead of a bare block -------------
{
    proto sub s3($) {*}
    multi sub s3(Int $x) { "outer" }
    sub s3-caller() {
        proto sub s3($) {*}
        multi sub s3(Int $x) { "inner" }
        s3(5);
    }
    is s3-caller(), "inner", "an inner proto+multi in a sub body shadows the outer one";
    is s3(5), "outer", "the outer proto+multi survives the sub body";
}

# --- 4. a differently-shaped inner proto (`|`) shadows just as well ---------
{
    proto sub s4($) {*}
    multi sub s4(Int $x) { "outer" }
    my $inner;
    {
        proto sub s4(|) {*}
        multi sub s4(Int $x) { "inner" }
        $inner = s4(5);
    }
    is $inner, "inner", "an inner proto with a different signature shadows too";
    is s4(5), "outer", "the outer proto comes back after that block";
}

# --- 5. an inner `multi` with NO inner proto EXTENDS the outer proto --------
# This is the common Raku pattern: a nested scope adds a candidate to the
# proto already in scope. It must keep working.
{
    proto sub s5($) {*}
    multi sub s5(Int $x) { "outer-int" }
    my @seen;
    {
        multi sub s5(Str $x) { "inner-str" }
        @seen = s5(5), s5("a");
    }
    is @seen.join("|"), "outer-int|inner-str",
        "an inner multi adds a candidate to the enclosing proto";
    is s5(5), "outer-int", "the outer candidate still answers after the block";
}

# --- 6. an inner proto shadowing an outer *single* sub ----------------------
{
    sub s6($x) { "outer-single" }
    my $inner;
    {
        proto sub s6($) {*}
        multi sub s6(Int $x) { "inner-multi" }
        $inner = s6(5);
    }
    is $inner, "inner-multi", "an inner proto+multi shadows an outer single sub";
    is s6(5), "outer-single", "the outer single sub comes back after the block";
}

# --- 7. a genuine redeclaration inside ONE scope is still an error ----------
dies-ok { EVAL 'sub s7a() { proto d7a($) {*}; proto d7a($) {*}; multi d7a(Int $x) { 1 }; d7a(2) }; s7a()' },
    "two protos of one name in one lexical scope still redeclare";
dies-ok { EVAL 'sub s7b() { proto d7b($) {*}; sub d7b($x) { 1 }; d7b(2) }; s7b()' },
    "a proto plus a single sub of one name in one scope still redeclare";

# --- 8. `our proto` / `our multi` -------------------------------------------
# `our multi` needs an our-scoped proto at every scope level; with one it is
# fine, including inside a nested block.
{
    my $r;
    {
        our proto s8($) {*}
        our multi s8(Int $x) { "inner" }
        $r = s8(5);
    }
    is $r, "inner", "our proto + our multi inside a nested block works";
}
dies-ok { EVAL 'our multi d8(Int $x) { 1 }' },
    "a bare `our multi` with no our-scoped proto is still rejected";

# --- 9. operator names keep merging candidates across scopes ----------------
# Many modules independently add candidates under one operator name; that must
# stay a merge, not a shadow.
{
    multi sub infix:<mplsop>(Int $a, Int $b) { "ii" }
    multi sub infix:<mplsop>(Str $a, Str $b) { "ss" }
    my @seen;
    {
        multi sub infix:<mplsop>(Num $a, Num $b) { "nn" }
        @seen = (1e0 mplsop 2e0), (1 mplsop 2), ("a" mplsop "b");
    }
    is @seen.join("|"), "nn|ii|ss",
        "an inner operator candidate merges with the enclosing ones";
    is (1 mplsop 2), "ii", "the outer operator candidates survive the block";
}

# --- 10. `our` is not lexically shadowable ---------------------------------
# `our` installs the routine in the *package*, not in the lexical scope, so a
# second `our proto` for the same name is a genuine redeclaration however
# deeply nested its block is. raku: "Redeclaration of routine 'foo' (already
# defined in package GLOBAL)". The lexical-shadow exemption above must not
# cover it.
dies-ok { EVAL 'our proto d10a($) {*}; our multi d10a(Int $x) { "o" }; { our proto d10a($) {*}; our multi d10a(Int $x) { "i" } }' },
    "a nested `our proto` redeclaring a package-scoped one is rejected";
dies-ok { EVAL 'our proto d10b($) {*}; our multi d10b(Int $x) { "o" }; sub s10b() { our proto d10b($) {*}; our multi d10b(Int $x) { "i" } }; s10b()' },
    "an `our proto` inside a routine body redeclares the package one too";

# A `my`-scoped inner proto still shadows an outer `our` one: that one *is* a
# lexical declaration, and raku allows it.
{
    our proto s10c($) {*}
    our multi s10c(Int $x) { "outer" }
    my $inner;
    {
        proto s10c($) {*}
        multi s10c(Int $x) { "inner" }
        $inner = s10c(5);
    }
    is $inner, "inner", "a lexical proto still shadows an outer `our` proto";
    is s10c(5), "outer", "the `our` proto comes back after the block";
}
