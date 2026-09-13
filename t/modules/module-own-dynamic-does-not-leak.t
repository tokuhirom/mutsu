use v6;
use Test;

# A module's OWN file-scope `my $*x = ...` declaration must not leak into the
# importing scope. The module body runs against the CALLER's env (see
# `run_modules.rs`), and a dynamic the module declares for ITSELF was never
# moved out into `unit_lexicals` (dynamics are dynamically scoped by
# definition, so `collect_unit_lexical_names` explicitly excludes them) nor
# reverted by the plain-env restore (which must NOT revert a dynamic the
# CALLER itself owns and the module merely wrote to, #8229) -- so with no
# further handling it was simply left behind as a permanent binding forever.
#
# https://github.com/tokuhirom/mutsu/issues/8241
#
# The loads go through EVAL so the `use` really happens at run time, inside
# the importing dynamic scope; a plain top-level `use` runs at BEGIN, before
# any of this machinery is even reachable from user code.

plan 2;

use lib 't/lib/Issue8241';

{
    EVAL q[use UnitOwnDynamic];
    is $*MODULE_PRIVATE // "(undeclared)", "(undeclared)",
        "a `unit module`'s own `my \$*x` does not leak into the importer";
}

{
    EVAL q[use BareFileOwnDynamic];
    is $*MODULE_PRIVATE // "(undeclared)", "(undeclared)",
        "a bare-file module's own `my \$*x` does not leak into the importer";
}
