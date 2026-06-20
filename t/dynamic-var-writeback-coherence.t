use Test;

# Slice F (env<->locals coherence): a callee assigning to a dynamic variable
# (`$*foo`) declared in the caller writes the value into `env` by name, but the
# caller's local slot was kept coherent only by the reverse `sync_locals_from_env`
# pull. This pins that the call-site drains the dynamic write through to the
# caller's local slot, so the behavior no longer depends on the reverse pull.
# Run with `MUTSU_NO_REVERSE_SYNC=1` to confirm coherence without the reverse pull.
#
# NOTE: only single-level (the writer's immediate caller declared the dynamic) is
# covered. Multi-frame propagation (caller -> mid -> writer) still relies on the
# reverse pull; folding it in needs cross-frame retention, which conflicts with
# lazy iteration (the same wall as the 0-arg captured-write slice, PR #3317).

plan 10;

# Plain assignment to a caller-declared dynamic var.
sub setter() { $*shared = 5 }
sub outer-assign() { my $*shared = 99; setter(); $*shared }
is outer-assign(), 5, 'callee assignment to caller dynamic var is visible';

# Post-increment.
sub inc-it() { $*counter++ }
sub outer-inc() { my $*counter = 10; inc-it(); $*counter }
is outer-inc(), 11, 'post-increment of caller dynamic var is visible';

# Post-decrement.
sub dec-it() { $*counter-- }
sub outer-dec() { my $*counter = 10; dec-it(); $*counter }
is outer-dec(), 9, 'post-decrement of caller dynamic var is visible';

# Compound assignment.
sub add-it() { $*acc += 7 }
sub outer-add() { my $*acc = 100; add-it(); $*acc }
is outer-add(), 107, 'compound += of caller dynamic var is visible';

# Two dynamic vars written in one call.
sub set-two() { $*a = 1; $*b = 2 }
sub outer-two() { my $*a = 0; my $*b = 0; set-two(); "$*a/$*b" }
is outer-two(), "1/2", 'two caller dynamic vars written in one call';

# String value.
sub set-name() { $*name = "raku" }
sub outer-name() { my $*name = "x"; set-name(); $*name }
is outer-name(), "raku", 'string assignment to caller dynamic var is visible';

# Multiple writers, accumulating across calls.
sub bump() { $*total += 1 }
sub outer-multi() {
    my $*total = 0;
    bump(); bump(); bump();
    $*total
}
is outer-multi(), 3, 'repeated callee bumps accumulate in caller dynamic var';

# Caller slot stays coherent for a subsequent method call on it.
sub stretch() { $*word = $*word ~ "!!!" }
sub outer-method() { my $*word = "hi"; stretch(); $*word.chars }
is outer-method(), 5, 'caller dynamic var slot is coherent for a later method call';

# Read-modify-write from the value the callee left.
sub double() { $*v = $*v * 2 }
sub outer-rmw() { my $*v = 21; double(); $*v }
is outer-rmw(), 42, 'callee read-modify-write of caller dynamic var is visible';

# Dynamic var used inside a later arithmetic expression in the caller.
sub set-base() { $*base = 40 }
sub outer-expr() { my $*base = 0; set-base(); $*base + 2 }
is outer-expr(), 42, 'caller dynamic var slot is coherent in a later expression';
