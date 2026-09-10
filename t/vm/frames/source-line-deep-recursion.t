use v6;
use Test;

# Pin for the ?LINE-as-field + empty-overlay-tier reuse change:
# the per-statement source-line tracking must survive deep recursion
# (chain depth > MAX_OVERLAY_DEPTH) and frame returns, both on the
# frame-less VM fast paths and the slow dispatch paths.

plan 6;

# 1. Deep recursion (depth 40 > MAX_OVERLAY_DEPTH 16) still computes correctly.
sub deep(Int $n --> Int) {
    if $n <= 0 { return 0 }
    return 1 + deep($n - 1);
}
is deep(40), 40, 'deep recursion works past the overlay-depth limit';

# 2. After a deep recursive call returns, the current-line tracking must be
#    back at the caller: callframe(1).line seen by a sub called right after
#    the recursion is the callsite line below, not a callee-body line.
sub caller-line { callframe(1).line }
deep(20);
my $after-line = caller-line();    # <-- must report THIS line
is $after-line, 23, 'caller line restored after deep recursion';

# 3. callframe(0).line inside a sub called from a loop reflects the sub body line.
my @lines;
sub probe { @lines.push(callframe(0).line) }
for 1..2 { probe() }
is @lines.elems, 2, 'probe ran twice';
is @lines[0], @lines[1], 'callframe(0).line is stable across calls';

# 4. A callee that *writes* an outer variable (non-empty overlay tier) still
#    recurses correctly (the empty-tier reuse must not skip non-empty tiers).
my $acc = 0;
sub bump(Int $n) {
    $acc = $acc + 1;
    bump($n - 1) if $n > 0;
}
bump(30);
is $acc, 31, 'captured-outer writes survive deep recursion';

# 5. $?LINE is still the compile-time constant.
is $?LINE, 44, '$?LINE compile-time constant';
