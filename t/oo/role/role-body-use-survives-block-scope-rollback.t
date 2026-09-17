use lib 't/lib';
use Test;

# Regression (#8646, shape 2): a role's deferred-body `use` statement
# installs its imported routine under the ROLE's own qualified package name
# (RoleUse::Holder::infix:<role-use-cmp>/...) exactly once, memoized by role
# composition (Registry::composed_role_bodies). That one-time import ran
# from inside whatever bare `{ ... }` block happened to trigger the FIRST
# composition -- and an ordinary bare block unconditionally snapshots and
# restores the whole routine registry around its body
# (OpCode::BlockScope / Interpreter::restore_routine_registry), a mechanism
# completely separate from the `use`-triggered import-scope bracket. Without
# tracking the import as persistent the same way a genuine module's own
# top-level declarations are, the operator became unreachable ("Two terms in
# a row") for any LATER call to the role's methods made from a different
# bare block -- even though the registration itself was never lexically
# scoped to begin with.

use RoleUseMaker;

plan 2;

sub do-it() {
    my $holder = make-holder();
    $holder.insert(make-item(2));
    $holder.insert(make-item(1));
    return $holder.compare-first-two();
}

{
    is do-it(), Order::More,
        "a role's own method can call its use-imported operator from inside a bare block";
}
{
    is do-it(), Order::More,
        "...and still can after that first bare block's routine-registry rollback";
}
