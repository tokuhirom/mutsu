use v6;
use Test;

# A typed scalar `my` inside a plain mainline block must not leak its
# constraint onto a same-named variable outside the block (the bare-name-keyed
# constraint store was scope-blind —
# todo/deep/bare-name-type-constraint-store-is-scope-blind.md, issue 2
# "Mainline blocks"). Companion to t/typed-lexical-constraint-frame-scoped.t,
# which covers the routine-scoped half of the same fix.
#
# Scope: this covers only a genuine source `{ ... }` block (compiled to
# `OpCode::BlockScope`). The `if`/`unless`/`else` branch-body half
# (`OpCode::BlockLocalScope`) is now ALSO fixed, for the "fresh-after" shape
# (a typed `my` inside the branch, then a FRESH untyped `my` of the same name
# declared after the branch exits) — see ADR-0042 slice 1 (step 4) and
# t/typed-constraint-scope-matrix.t, which pins it (plus the ADR's container
# matrix). `while`/`for`/C-style-loop bodies (no scope-boundary opcode at
# all) remain unfixed for that same "fresh-after" shape in principle, though
# no live fresh-after repro is known for them either (every one tried during
# the ADR-0042 slice-1 session already passed). A SEPARATE, deeper "outer-first
# shadow" shape (an outer variable declared BEFORE the inner typed shadow,
# then reused after it exits) is unfixed for EVERY branch/loop construct
# including if/unless/else — see
# todo/deep/scoped-type-declaration-tags-the-shadowed-outer-value.md and
# t/typed-constraint-shadow-leak-unfixed.t.

plan 7;

# A pre-existing outer untyped $x must not be poisoned by a typed $x declared
# inside a bare block.
{
    my $x;
    { my Str $x = "a"; }
    lives-ok { $x = 42 }, 'bare-block-scoped my Str $x does not constrain outer $x';
    is $x, 42, 'outer $x holds the assigned Int';
}

# The literal repro shape from the todo: block runs and exits BEFORE the
# outer `my $x` is (re-)declared.
{
    { my Str $x = "a"; }
    my $x;
    lives-ok { $x = 42 }, 'block exiting before outer redeclaration does not leak';
}

# Enforcement INSIDE the block still works (env-scoped registration).
{
    { my Str $s = "a"; dies-ok { $s = 42 }, 'constraint enforced inside the declaring block'; }
}

# A closure escaping the block keeps enforcement through its captured env.
{
    my &esc;
    { my Str $c = "a"; &esc = sub { $c = 42 } }
    dies-ok { esc() }, 'escaped closure still enforces the dead block constraint';
}

# Nested block: the leak must not survive even one extra level of nesting.
{
    my $x;
    { { my Str $x = "a"; } }
    lives-ok { $x = 42 }, 'nested-block-scoped my Str $x does not constrain outer $x';
}

# An outer typed lexical keeps enforcement while a block shadows the name.
{
    my Str $o = "s";
    { my Int $o = 1; }
    throws-like { $o = 42 }, Exception, message => /'expected Str'/,
        'outer Str constraint intact after block-scoped Int shadow';
}
