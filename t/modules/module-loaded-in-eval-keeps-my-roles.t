use v6;
use MONKEY-SEE-NO-EVAL;
use Test;

# A `my role` declared by an EVAL string is EVAL-local and is torn down with it.
# One declared by a MODULE that the EVAL happened to load is not: the module's
# compunit outlives the EVAL, and a later re-`use` re-runs only its `sub EXPORT`,
# never its mainline — so the role has to still be registered.
#
# mutsu tagged every `my role` declared while it was inside an EVAL, module loads
# included, and dropped it when the EVAL ended. The second `EVAL 'use if'` in a
# process then died with "Slang activation: 'Actions' is not a known role",
# because the `if` pragma's EXPORT re-reads the role its mainline declared
# (ADR-0098).

plan 2;

# The fixture's `sub EXPORT` dies unless its mainline's `my role Marked` is
# still resolvable, so "lives" is the whole assertion.
my $load = q[use lib 't/lib'; use EvalModuleMyRole];

lives-ok { EVAL($load) },
   'the module loads inside an EVAL and its lexical role is there';

lives-ok { EVAL($load) },
   'a second EVAL re-runs EXPORT against the role the first load registered';

# The EVAL-local half of the rule is untouched — a role the EVAL STRING itself
# declares is still torn down — and stays pinned where it already was
# (`roast/S11-modules/lexical.t`, `t/modules/eval-stub-package-does-not-leak.t`).
