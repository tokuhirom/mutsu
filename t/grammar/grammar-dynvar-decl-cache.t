use Test;

# Regression test for the per-package grammar dynvar-decl memo (#8510):
# Interpreter::establish_grammar_dynamic_vars used to re-scan every
# registered token/rule definition in the whole program on every single
# .parse()/.subparse() call, to build the target package's `:my $*/%*/@*…`
# declaration table. That scan is now memoized per package, keyed by the
# TOKEN_DEFS_GEN generation counter every token/rule registration bumps.
# This guards that:
#   1. a grammar declaring a rule-scoped dynamic variable still parses
#      correctly (cache preserves semantics),
#   2. repeated parses of the same grammar stay correct (cache reuse), and
#   3. declaring a NEW token/rule elsewhere (bumping the generation) does
#      not leave a stale per-package cache entry — a later parse of a
#      DIFFERENT grammar, whose own dynvar-declaring rule is registered
#      after the first grammar's cache entry already exists, still sees it.

plan 5;

grammar Tagged {
    token TOP(:$*prefix = 'x') { <part>+ % ',' }
    token part { :my $*seen = 0; \w+ { $*seen = 1; } <?{ $*seen }> }
}

is Tagged.parse("ab,cd,ef").Str, "ab,cd,ef", "rule-scoped dynvar parse correct";

# Repeated parses (exercises the per-package memo across many calls).
my @results;
for 1..50 {
    @results.push: Tagged.parse("foo,bar,baz").Str;
}
is @results.elems, 50, "50 repeated parses ran";
is @results.all eq "foo,bar,baz", True, "every repeated parse correct";

# Declaring more tokens/rules elsewhere (bumps TOKEN_DEFS_GEN -> the
# per-package memo is invalidated) must not change parse results.
class Extra1 { method x { 1 } }
grammar Unrelated { token TOP { \d+ } }
is Tagged.parse("still,works").Str, "still,works",
    "parse still correct after unrelated token/rule declarations";

# A DIFFERENT grammar, registered AFTER Tagged's dynvar-decl cache entry
# already exists, must get its own correct table — not Tagged's cached one,
# and not a stale empty one from before its own rule was registered.
grammar AlsoTagged {
    token TOP { :my $*digits-seen = 0; \d+ { $*digits-seen = 1; } <?{ $*digits-seen }> }
}
is AlsoTagged.parse("12345").Str, "12345",
    "a different grammar's own dynvar decl is visible despite an existing cache entry";
