use Test;

# Pin for the (B) per-store env-write gate sigilless typed-bind fix
# (docs/lexical-scope-slot-campaign.md, "(B) per-store env-write gate — burndown",
# sigilless cluster #1).
#
# A *typed* sigilless declaration (`my Int \d := 7`) used to compile as a bare
# VarDecl with no sigilless marker, so the bare-word read `d` resolved via
# GetBareWord (env lookup) instead of GetLocal (slot). Under the
# MUTSU_GATE_LOCAL_ENV_WRITE gate the env mirror is stale, so `d` read as `Any`.
# The fix registers a typed sigilless name in `sigilless_locals` via the new
# non-readonly `MarkSigilless` marker, making the read slot-addressed. This passes
# gate-OFF (the default) and would fail gate-ON before the fix.

plan 5;

my Int \d := 7;
is d, 7, 'typed sigilless := reads from its slot';

my Str \s := 'hi';
is s, 'hi', 'typed sigilless := with Str constraint reads from its slot';

my Int \e = 42;
is e, 42, 'typed sigilless = reads from its slot';

# Untyped forms must keep working (regression guard for the shared code path).
my \u := 99;
is u, 99, 'untyped sigilless := still reads correctly';

my \v = 3, 4, 5;
is v[1], 4, 'untyped sigilless = keeps its comma-list RHS';
