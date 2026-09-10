use v6;
use Test;

# The "mark context" one-shot VM flag family (bind_context,
# scalar_bind_context, param_raw_bind_context, bound_decont_active,
# rebind_context, constant_context, array_share_context /
# array_share_source, explicit_initializer_context, vardecl_context) is set
# immediately before a `:=`/vardecl target's own store op and is meant to be
# consumed by that VERY NEXT store. Two boundaries have to isolate it so a
# pending mark never reaches unrelated code:
#
#   * an ordinary call (`MarkContextGuard`) -- pinned by
#     t/bind-through-call-boundary-vardecl-leak.t;
#   * a NESTED RUN (`EVAL`, a `dies-ok`/`lives-ok` block) -- pinned here.
#
# The family is stored packed in one word (src/runtime/mark_context.rs), so
# these tests also guard the packing: a mask that aliased two flags, or a
# save/restore that dropped one, shows up as a typed native array that never
# materialized from its Range (the classic symptom -- a store treated as a
# bind skips the materialization) or as an outer bind that stopped binding.

plan 7;

# A nested run (EVAL) sitting between a bind's mark and its consuming store
# must not run with the mark still set.
my @evaled := EVAL 'my uint8 @state = 0..5; @state[2] = 99; @state';
is @evaled.join(','), '0,1,99,3,4,5',
    'a typed-array decl inside an EVAL under a pending bind materializes';

# ...and the outer bind is still a real bind afterwards: the restore has to
# put the caller's flags back, not leave them cleared.
is @evaled.of.raku, 'uint8',
    'the outer bind still binds the nested run result';

# The same for a `dies-ok`-style nested run (vm_run_loop's other entry).
my @after-block := do {
    lives-ok { my uint8 @inner = 0..3; @inner[1] = 7 },
        'a typed-array decl inside a lives-ok block under a pending bind works';
    my uint8 @outer = 0..5;
    @outer[2] = 99;
    @outer;
};
is @after-block.join(','), '0,1,99,3,4,5',
    'the enclosing bind survives a nested run in its RHS';

# `param_raw_bind_context` is the family member the nested-run boundary used
# NOT to isolate (only the ordinary-call guard did). A sigilless loop
# parameter's bind sets it; an EVAL inside that loop body is a nested run
# started while it is live.
my @seen;
for 1, 2 -> \v {
    @seen.push: EVAL 'my uint8 @s = 0..3; @s[1] = 9; @s.join(",")';
    @seen.push: v;
}
is @seen.join('|'), '0,9,2,3|1|0,9,2,3|2',
    'a nested run under a live sigilless-bind mark isolates it, and the alias still binds';

# The array-share member carries a name (not just a bit); a nested run must
# save and restore that too, or `$n = @z` after one stops sharing.
my @z = 1, 2, 3;
my $n = @z;
EVAL '1';
@z.push: 4;
is $n.elems, 4, 'a `$scalar = @array` share survives a nested run';
is $n.join(','), '1,2,3,4', 'and still reflects later pushes to the source';
