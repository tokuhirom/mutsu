use v6.c;

# Deliberately mismatched: the compunit is `use`d as `ReusedRoleCommon` (this
# file's name) but declares its OWN package under a different name -- exactly
# like the real-world case this pins (`IP::Addr::Common.rakumod` declares
# `unit module IP::Addr::Const;`). That mismatch is essential to the bug: see
# ../modules/module-reused-role-does-in-nested-decl.t.
unit module ReusedRoleInternalName;

role Reuse-Role is export {
    method greet { "hi-from-role" }
}
