use Test;

# A value-position block written in the source (`do { ... }`, a labelled
# `L: { ... }`, the inline `lazy`/`sink`/`quietly` blocks) is a Raku block, so a
# `my` declared in it is lexical to it and must not be visible by name after it
# exits (#9897, ADR-0076 §6). mutsu's `OpCode::DoBlockExpr` used to leave every
# such declaration in the enclosing env, where `$::("name")` found it.
#
# Every expectation below was measured against Rakudo first; raku is the
# oracle and this file passes verbatim under both.

plan 24;

# --- 1. each sigil stays inside the do block ---------------------------------
{
    my $v = do { my $zz = 9; my @aa = 1, 2; my %hh = a => 1; my &ff = { 3 }; 1 };
    is $v, 1, "the do block still yields its value";
    is (try $::("zz")) // "gone", "gone", 'a scalar declared in a do block does not leak';
    is (try $::("@aa")) // "gone", "gone", 'an array declared in a do block does not leak';
    is (try $::("%hh")) // "gone", "gone", 'a hash declared in a do block does not leak';
    is (try $::("&ff")) // "gone", "gone", 'a &-variable declared in a do block does not leak';
}

# --- 2. a shadowing declaration gives the outer value back --------------------
{
    my $out = 5;
    my $r = do { my $out = 7; $out };
    is $r, 7, "the inner declaration answers inside the block";
    is $out, 5, "the outer variable is untouched after the block";
}

# --- 3. outer mutations and escaping closures still work ----------------------
{
    my $w = 1;
    do { $w = 2 };
    is $w, 2, "assigning an outer variable inside a do block persists";

    my $c = do { my $q = 10; -> { $q++ } };
    $c();
    is $c(), 11, "a closure escaping the block keeps its captured variable";
}

# --- 4. the other source-block forms ------------------------------------------
{
    my $l = lazy { my $lz = 3; 1, 2 };
    is (try $::("lz")) // "gone", "gone", "a lazy block's declaration does not leak";
    quietly { my $qt = 4 };
    is (try $::("qt")) // "gone", "gone", "a quietly block's declaration does not leak";
    L: do { my $lb = 5 };
    is (try $::("lb")) // "gone", "gone", "a labelled do block's declaration does not leak";
}

# --- 5. writes to bindings that outlive the block persist ---------------------
{
    my $*A = 42;
    do { $*A++ };
    is $*A, 43, "a write to an outer dynamic variable persists";
    my $v = do { my $*ee = 5; $*ee };
    is $v, 5, "a dynamic variable declared in the block answers inside it";
    is (try $*ee) // "gone", "gone", "a dynamic variable declared in the block does not leak";
    do { "abc" ~~ /b/ };
    is ~$/, "b", 'the match variable set in a do block is the outer $/';
    do { try die "boom" };
    is $!.message, "boom", 'an error caught in a do block sets the outer $!';
}

# --- 6. only the block's own scope is reverted --------------------------------
{
    # The inner declarations are used, or Rakudo's optimizer lowers them away.
    my @seen;
    my $x = 1;
    do { given 1 { my $x = 2; @seen.push: $x }; $x = 5 };
    is $x, 5, "a nested given body's declaration does not revert a later outer write";
    my $y = 1;
    do { { my $y = 2; @seen.push: $y }; $y = 5 };
    is $y, 5, "a nested block's declaration does not revert a later outer write";
    my $z = 1;
    do { for 1 { my $z = 2; @seen.push: $z }; $z = 5 };
    is $z, 5, "a nested loop body's declaration does not revert a later outer write";
    my Str $o = "s";
    do { my Int $o = 1; @seen.push: $o };
    lives-ok { $o = "t" }, "an inner typed declaration does not leak its type constraint";
}

# --- 7. a method in a role body's do block keeps its capture ------------------
{
    role Mx { do { my $v = 9; method mv { $v } } }
    is (1 but Mx).mv, 9, "a mixin sees the do block's lexical";
    is (try $::("v")) // "gone", "gone", "the role body's do-block lexical does not leak";
}

# --- 8. a desugared statement list is not a block ----------------------------
{
    my $x = $( my $ctx = 6; $ctx );
    is $ctx, 6, 'a $( ... ) contextualizer declares into the enclosing scope';
}
