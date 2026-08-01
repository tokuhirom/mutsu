use v6;
use Test;

# mutsu provides the `Test` module natively (`src/runtime/test_functions/`), and
# the statement-call path dispatched every name in `is_test_function_name()` to
# those Rust routines BEFORE resolving user routines, and without any gate. So a
# module exporting its own `ok`/`is`/... was silently overruled: the call went to
# mutsu's TAP implementation instead.
#
# It is a nasty failure to diagnose, because the two implementations then keep
# separate counters. Loading rakudo's real Test.rakumod under an alias produced
#
#     1..3
#     ok 1 - first     <- native handler
#     ok 1 - like      <- the module's own routine, its own counter
#     ok 2 - third     <- native handler again
#
# which reads as a stale module lexical rather than as two live implementations.
# (todo/tickets/vendor-real-test-module.md)
#
# The rule is the one from the qualified-call guard: decide on whether a
# *declaration* exists, not on whether the name is a builtin. `use Test` is
# intercepted natively and registers no routines, so the ordinary path has
# nothing to compete with and is unaffected -- pinned below too.
#
# Run in a subprocess: importing the fixture here would shadow the very `ok`
# this file's own assertions use.

plan 6;

my $exe = $*EXECUTABLE;

sub run-snippet($code) {
    my $r = run($exe, '-I', 't/lib', '-e', $code, :out, :err);
    my $out = $r.out.slurp(:close);
    my $err = $r.err.slurp(:close);
    $r.exitcode == 0 ?? $out.trim !! "$out.trim() [exit {$r.exitcode}] $err.trim()"
}

is run-snippet('use ShadowingTap; ok 1, "a"; ok 0, "b"; done-testing'),
    "MINE ok 1 - a\nMINE not ok 2 - b\nMINE done, ran 2",
    'an imported `ok` wins over the native Test provider';

is run-snippet('use ShadowingTap; plan 2; is 2, 2, "x"; is 1, 2, "y"'),
    "MINE plan 2\nMINE ok 1 - x\nMINE not ok 2 - y",
    'so do an imported `plan` and `is`';

is run-snippet('use ShadowingTap; diag "hello"'), 'MINE diag hello',
    'and an imported `diag`, which writes to stdout here rather than stderr';

# The counters must stay in one implementation: the bug showed up as the
# module's own counter falling behind because half the calls never reached it.
is run-snippet('use ShadowingTap; ok 1, "a"; is 1, 1, "b"; ok 1, "c"; done-testing'),
    "MINE ok 1 - a\nMINE ok 2 - b\nMINE ok 3 - c\nMINE done, ran 3",
    'a mix of imported test routines shares one counter';

# Regression guard: the ordinary `use Test` path is untouched.
is run-snippet('use Test; plan 2; ok 1, "native"; is 3, 3, "native too"'),
    "1..2\nok 1 - native\nok 2 - native too",
    'plain `use Test` still reaches the native provider';

is run-snippet('use Test; plan 1; ok 1, "still numbered from the native counter"'),
    "1..1\nok 1 - still numbered from the native counter",
    'and keeps numbering its own tests';
