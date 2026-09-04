use Test;

# A value-position `do { ... }` block is a block, so a routine declared inside
# it is lexical to it: it must not leak into the enclosing scope, and an outer
# routine of the same name must come back when the block exits. mutsu's
# `OpCode::DoBlockExpr` took none of the routine-registry snapshot/restore that
# `OpCode::BlockScope` (the statement-form bare block), every routine call and
# every for-loop body already take, so a `do`-block `sub` permanently replaced
# the outer one.
#
# Every expectation below was measured against Rakudo v2026.06 (2026-09-04)
# first; raku is the oracle and this file passes verbatim under both.

plan 11;

# --- 1. a plain sub in a `do` block shadows, then gives the outer one back ---
{
    sub d1() { "outer" }
    my $inner = do { sub d1() { "inner" }; d1() };
    is $inner, "inner", "the do-block sub answers inside the block";
    is d1(), "outer", "the outer sub comes back after the do block";
}

# --- 2. proto + multi behave the same way -----------------------------------
{
    proto d2($) {*}
    multi d2(Int $x) { "outer" }
    my $inner = do { proto d2($) {*}; multi d2(Int $x) { "inner" }; d2(1) };
    is $inner, "inner", "a do-block proto+multi family answers inside the block";
    is d2(1), "outer", "the outer proto+multi family comes back";
}

# --- 3. a do-block routine does not leak at all -----------------------------
dies-ok { EVAL 'my $r = do { sub d3() { 1 }; d3() }; d3()' },
    "a do-block sub is not visible after the block";
dies-ok { EVAL 'my $r = do { proto d3b($) {*}; multi d3b(Int $x) { 1 }; d3b(1) }; d3b(1)' },
    "a do-block proto+multi family is not visible after the block";

# --- 4. nesting ------------------------------------------------------------
{
    sub d4() { "outer" }
    my @seen;
    my $v = do {
        sub d4() { "mid" }
        @seen.push: d4();
        @seen.push: do { sub d4() { "deep" }; d4() };
        @seen.push: d4();
        "done";
    };
    is $v, "done", "the nested do block still yields its own value";
    is @seen.join("|"), "mid|deep|mid", "each do block sees its own d4";
    is d4(), "outer", "the mainline d4 survives both nested blocks";
}

# --- 5. a routine taken as a VALUE out of the block stays callable ----------
# Scoping the declaration must not invalidate a reference that escaped.
{
    my &d5 = do { sub d5-inner() { "escaped" }; &d5-inner };
    is d5(), "escaped", "a routine value returned from a do block is callable";
}

# --- 6. the statement-form bare block was already correct; keep it pinned ---
{
    sub d6() { "outer" }
    my $inner;
    { sub d6() { "inner" }; $inner = d6(); }
    is "$inner/{d6()}", "inner/outer", "the statement-form block still scopes";
}
