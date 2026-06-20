use Test;

# Slice F (env<->locals coherence): a dynamic variable (`$*foo`) is dynamic-scope.
# Reads of a `my $*foo`-declared dynamic var now go straight to `env` by name
# (the compiler emits GetGlobal, not GetLocal — `compile_expr_var`), and dynamic
# writes always reach env, so a callee's `$*foo = …` is visible to the declaring
# scope WITHOUT the reverse `sync_locals_from_env` pull. Run with
# `MUTSU_NO_REVERSE_SYNC=1` to confirm coherence without the reverse pull.
#
# Reading dynamic vars from env (vs the single-level call-site drain of PR #3320)
# also makes MULTI-FRAME propagation (caller -> mid -> writer) work, since env is
# the one dynamic-scope store — see the multi-frame case below.

plan 11;

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

# Multi-frame propagation: the writer is two frames below the declaring scope.
# (The earlier single-level call-site drain could not do this; reading from env
# does, because env is the one dynamic-scope store.)
sub deep-writer() { $*md = 99 }
sub middle() { deep-writer() }
sub outer-multiframe() { my $*md = 0; middle(); $*md }
is outer-multiframe(), 99, 'multi-frame callee write to caller dynamic var is visible';
