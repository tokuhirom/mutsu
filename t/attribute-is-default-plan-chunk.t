use Test;

# `is default(...)` on a class attribute is now precompiled into a child
# chunk at plan-lowering time (ADR-0019 D2c) instead of being re-compiled by
# `eval_block_value` every time the class body registers. Exercise the
# non-literal-expression path (forces the `Compiled` chunk, not the `Literal`
# fast path) and repeated registration of the same declaration site with
# different closed-over state, to make sure the precompiled chunk resolves
# free variables dynamically per registration rather than baking in a stale
# value.

# Non-literal is-default expression on a class attribute.
{
    my $base = 10;
    class WithComputedDefault {
        has $.x is default($base + 5) is rw;
    }
    my $obj = WithComputedDefault.new;
    is $obj.x, 15, 'is default(...) with a non-literal expr evaluates correctly';
    $obj.x = Nil;
    is $obj.x, 15, 'assigning Nil restores the computed default';
}

# The same declaration site re-registered across loop iterations must see
# each iteration's own closed-over value, not a value baked in once.
{
    my @firsts;
    my @restored;
    for 1..3 -> $n {
        my class Repeated {
            has $.y is default($n * 100) is rw;
        }
        my $obj = Repeated.new;
        @firsts.push($obj.y);
        $obj.y = Nil;
        @restored.push($obj.y);
    }
    is-deeply @firsts, [100, 200, 300],
        'is default(...) picks up each registration\'s own loop-variable binding';
    is-deeply @restored, [100, 200, 300],
        'Nil-restore after repeated registration uses the matching binding too';
}

# `is default(...)` alongside an explicit `=` initializer: the initializer
# wins on construction, the is-default value wins after Nil is assigned.
{
    my $d = 7;
    class WithBoth {
        has $.z is default($d * 2) is rw = 1;
    }
    my $obj = WithBoth.new;
    is $obj.z, 1, 'explicit initializer wins over is default(...) on construction';
    $obj.z = Nil;
    is $obj.z, 14, 'is default(...) wins after Nil is assigned';
}

# A role attribute's `is default(...)` still evaluates correctly (not
# migrated to a precompiled chunk in this slice — role attribute defaults are
# deferred to composition time via the Expr-valued registry table, ADR-0019
# D2c-3), so this is a regression guard that the class-side change didn't
# disturb it. (Nil-restore of a role-composed attribute's `is default(...)`
# is a separate, pre-existing bug — reproduces identically before this
# change — and is out of scope here.)
{
    role R {
        has $.w is default(21) is rw;
    }
    class Consumer does R { }
    my $obj = Consumer.new;
    is $obj.w, 21, 'role attribute is default(...) still evaluates at composition/construction';
}

done-testing;
