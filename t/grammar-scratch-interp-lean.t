use Test;

# Regression test for perf-regex-scratch-skip-heavy-init: regex/grammar
# sub-interpreters are now built via a lightweight constructor that skips the
# heavy per-process environment setup (%*ENV sweep, the process-global enum/
# dynamic base, $*REPO, the default site repo) that `Interpreter::new` does for
# a top-level interpreter. This guards that constructs which run inside such a
# scratch interpreter still behave correctly:
#   1. grammar-with-actions parses (the full-registry scratch path),
#   2. built-in enum constants inside a regex code assertion (the lean scratch
#      path, whose registry inherits the parent's enum types),
#   3. dynamic variables ($*OUT etc.) inside a regex closure still resolve via
#      the inherited env.

plan 6;

# 1. grammar with a negated subrule + actions (the zef-shape parse path).
grammar Dotted {
    token TOP  { <part>+ % '::' }
    token part { <-[:]>+ }
}
class Act {
    method TOP($/)  { make $<part>.map(*.Str).join("-") }
}
is Dotted.parse("Foo::Bar::Baz", :actions(Act)).made, "Foo-Bar-Baz",
    "grammar+actions parse works in scratch interpreter";

# Repeated parses (each spawns fresh scratch interpreters).
my @r;
for 1..20 { @r.push: Dotted.parse("A::B::C", :actions(Act)).made }
is @r.all eq "A-B-C", True, "20 repeated grammar+actions parses correct";

# 2. built-in enum constants inside a regex code assertion (lean scratch path).
ok ("x" ~~ / <?{ Order::Less.defined }> . /).defined,
    "built-in Order enum visible inside a regex assertion";
is Signal::SIGINT.Int, 2, "built-in Signal enum still resolves";

# 3. user-declared enum visible inside a regex closure run by a scratch interp.
my enum Color <Red Green Blue>;
ok ("y" ~~ / <?{ Green == 1 }> . /).defined,
    "user-declared enum visible inside a regex assertion";

# 4. interpolated closure that reads a lexical (env inherited by the scratch).
my $needle = "cat";
ok ("a cat here" ~~ / <{ $needle }> /).defined,
    "regex closure reads an outer lexical via the inherited env";
