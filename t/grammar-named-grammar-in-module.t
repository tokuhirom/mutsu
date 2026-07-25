use v6;
use Test;

# A grammar declared with the same name as the built-in `Grammar`, but inside a
# module, must still be recognised as a grammar: it qualifies to `Mod::Grammar`,
# which is a distinct type that inherits the built-in Grammar. Before the fix the
# parser dropped the default `Grammar` parent for any decl literally named
# `Grammar`, so `Mod::Grammar` had no Grammar ancestor and `.parse` dispatch /
# `class_is_grammar` failed for it.

plan 3;

# The named-Grammar-in-module case is exercised through EVAL of a unit module so
# the qualification (`GM::Grammar`) actually happens.
my $mod = q:to/MOD/;
    unit module GM;
    grammar Grammar {
        token TOP { \d+ }
    }
    our sub parse-it($s) is export { Grammar.parse($s) }
    MOD

my &parse-it = EVAL $mod ~ "\n&parse-it";
ok parse-it("123").defined, 'module-local `grammar Grammar` parses';
is ~parse-it("123"), '123', 'the whole input matched';
nok parse-it("abc").defined, 'a non-match still fails cleanly';
