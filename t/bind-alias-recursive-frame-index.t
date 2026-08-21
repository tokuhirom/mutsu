use Test;
plan 2;

# Pin for todo/tickets/bind-alias-saved-locals-wrong-frame-index.md.
#
# `Interpreter::propagate_bind_to_ancestor_frames` (src/vm/vm_var_assign_ops.rs)
# used to also patch `frame.saved_locals[i]` for every ancestor call frame
# that owned the bind's source name, where `i` was found by searching the
# CURRENTLY EXECUTING frame's own `code.locals`. That index only happened to
# be correct when the ancestor frame was a *recursive* invocation of the
# exact same compiled function (so the two locals layouts coincide by
# construction) -- the one case where the patch was not unconditionally a
# no-op. These two subtests exercise exactly that shape (a `:=` bind
# performed at the base case of a recursive sub, naming that same sub's own
# local, so the propagate loop's by-name match hits every ancestor
# invocation of the sub) for both the scalar-rebind bind path and the
# whole-container (`@`) bind path.
#
# NB: each subtest's sub/array live at the file's mainline scope with
# distinct names, and the whole-container (`@`) subtest reads `@v[0]` right
# after the if/else on every call (via the `.push(@v[0])` below). Both are
# load-bearing to avoid a separate, pre-existing, unrelated stack overflow:
# a recursive sub that builds a trailing-comma list literal of its own
# parameter into a `my @` local (`my @v = ($n,);`) and then never reads that
# local again before the recursive call returns crashes with a native Rust
# stack overflow, independent of `:=` bind or block scoping entirely (both
# were red herrings from this ticket's original investigation). See
# todo/deep/recursive-sub-trailing-comma-array-literal-of-own-param-stack-overflow.md.
#
# IMPORTANT: these subtests intentionally do NOT assert raku's answer
# (`[999 1 2 3]`) -- mutsu currently answers `[999 999 999 999]` for both,
# a *separate*, out-of-scope bug in the `saved_env`-by-name splice that
# `propagate_bind_to_ancestor_frames` still performs (it does not
# distinguish "the one true declaring frame of a captured free variable"
# from "an unrelated ancestor invocation of the same recursive function
# that independently declared a same-named local"). That bug is also
# tracked in the todo/deep file above.
#
# What these subtests DO pin is that removing the `saved_locals[i]` patch
# does not change mutsu's output at all: the investigation for the ticket
# above built and ran both variants (patch present vs. patch's body deleted)
# against this exact code and got byte-identical output both times, which is
# the evidence that justified deleting it outright.

my @rec-scalar-levels;
sub rec-scalar(Int $n) {
    my $v = $n;
    if $n > 0 {
        rec-scalar($n - 1);
    } else {
        my $x := $v;
        $x = 999;
    }
    @rec-scalar-levels.push($v);
}
rec-scalar(3);
is @rec-scalar-levels, (999, 999, 999, 999),
    'recursive scalar := bind at the base case: current mutsu output pinned';

my @rec-array-levels;
sub rec-array(Int $n) {
    my @v = ($n,);
    if $n > 0 {
        rec-array($n - 1);
    } else {
        my @x := @v;
        @x[0] = 999;
    }
    @rec-array-levels.push(@v[0]);
}
rec-array(3);
is @rec-array-levels, (999, 999, 999, 999),
    'recursive whole-container @ := bind at the base case: current mutsu output pinned';
