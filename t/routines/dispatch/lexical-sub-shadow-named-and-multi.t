use v6;
use Test;

# #9080: two shapes where a routine-local `my sub` shadowing an outer routine
# of the same name went wrong from the SECOND call of the enclosing routine
# on -- the first call was always correct, which is what let both bugs hide
# behind the (positional, zero-arg) pinned coverage in
# `lexical-sub-dispatch-memo.t`.

# 1. Named-parameter call: the light-call fast path skips the routine-scope
#    registry snapshot/restore that a body declaring an inner `my sub` relies
#    on to take the declaration away again on return. Left unguarded, the
#    inner `my sub`'s resolution answered every OUTER call site of the same
#    name too, from the routine's second call onward (`has_inner_subs` now
#    excludes such a body from the named light-call path, matching the
#    positional-light path's existing exclusion).
sub named-helper(:$v) { "outer:$v" }
sub named-shadowing(:$v) {
    my sub named-helper(:$v) { "inner:$v" }
    named-helper(:$v);
}
my @named;
for ^3 -> $v {
    @named.push: named-helper(:$v);
    @named.push: named-shadowing(:$v);
}
is @named.join(','), 'outer:0,inner:0,outer:1,inner:1,outer:2,inner:2',
    'a my sub shadowing a named-param outer sub resolves correctly on every call';

# 2. The shadowed name is an outer MULTI: a routine-local `my sub` of the
#    same base name must take the name over completely inside the routine,
#    on every call -- not just the first. The routine's first call runs the
#    full sub-registration path, which hides the outer multi's candidate
#    keys while the `my sub` is in scope; every call after that took a
#    "derive-once" fast path that skipped that step (the registry had
#    already been restored to a state where the multi's keys were back), so
#    the inner declaration was ignored and the outer multi answered instead.
multi sub kind(Int $) { 'Int' }
multi sub kind(Str $) { 'Str' }
sub shadow-multi($x) {
    my sub kind($) { 'inner' }
    kind($x);
}
my @multi;
for 1, 'a', 3 -> $x {
    @multi.push: shadow-multi($x);
    @multi.push: kind($x);
}
is @multi.join(','), 'inner,Int,inner,Str,inner,Int',
    'a my sub shadowing an outer multi of the same name resolves correctly on every call';

done-testing;
