use Test;

# A named routine's code object is rebuilt fresh from the registry at every
# call (for `callframe().code`) and at every bare `&name` mention from a
# different scope, rather than kept as one persistent object. So composing a
# role onto it (`.^mixin(Role)`, or a trait handler's `$r does Role` -- the
# mechanism `Test.rakumod` uses to mark `is test-assertion` routines
# introspectably) used to vanish on the next rebuild: `&foo` right after the
# declaration saw the role, but `callframe(N).code` from inside a call to
# `foo`, or `&foo` mentioned from a different scope, did not. See
# news/2026-08/test-assertion-trait-is-not-introspectable.md.

plan 6;

use nqp;

role is-marked { method is-marked(--> True) { } }
multi sub trait_mod:<is>(Routine:D $r, :$marked!) { $r does is-marked }
sub bar() is marked { 1 }

# callframe(0).code, from INSIDE the routine's own call, must still carry the
# role -- this is the mechanism Test.rakumod's caller-blaming walk depends on.
sub marked-probe() is marked {
    return nqp::can(callframe(0).code, "is-marked").so;
}
ok marked-probe(), 'callframe(0).code carries a role mixed onto the calling routine';

# The caller's callframe (as seen from a callee `bar` invokes) also carries it.
role is-outer-marked { method is-outer-marked(--> True) { } }
multi sub trait_mod:<is>(Routine:D $r, :$outer-marked!) { $r does is-outer-marked }
sub outer-marked-caller() is outer-marked {
    inner-probe2();
}
sub inner-probe2() {
    return nqp::can(callframe(1).code, "is-outer-marked").so;
}
ok outer-marked-caller(), 'callframe(1).code carries a role mixed onto the caller';

# `&name` mentioned from a DIFFERENT scope than the declaring one must also
# see it (the declaring scope already worked via a simpler env writeback).
sub other-scope-check() {
    return &bar.can("is-marked").Bool;
}
ok other-scope-check(), '&name from a different scope still carries the mixed-in role';

# The routine is still callable and returns its normal result, unaffected by
# carrying the extra role.
is bar(), 1, 'the routine still runs normally after being mixed with a role';

# A routine that was never mixed with anything does not spuriously pick up a
# role recorded for some OTHER, unrelated routine (the record is keyed by
# "package::name", not a global flag).
sub plain() {
    return nqp::can(callframe(0).code, "is-marked").so;
}
nok plain(), 'an unrelated, never-mixed routine does not spuriously carry a role recorded for another routine';

# .^mixin(Role) (not just a trait handler's `does`) also persists.
role is-zorked { method is-zorked(--> True) { } }
sub zork() {
    return nqp::can(callframe(0).code, "is-zorked").so;
}
&zork.^mixin(is-zorked);
ok zork(), '.^mixin(Role) on a routine also survives rebuild into callframe().code';

done-testing;
