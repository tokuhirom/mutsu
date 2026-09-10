use v6;
use MONKEY-SEE-NO-EVAL;
use Test;

# `EVAL` compiles a new compunit nested inside the caller's scope, so a `sub f`
# declared in it SHADOWS an outer `f` -- it can never be a redeclaration of one.
# Only a name declared inside the same EVAL still conflicts.
#
# mutsu had the exemption, but only for the `&name` env binding. The
# redeclaration checks also consult the routine REGISTRY, and the two records do
# not always agree about what exists: a `my sub` declared inside a block that
# runs as a *callable* leaves a registry entry reachable afterwards without
# leaving an `&name` in any visible env tier. An EVAL'd `sub` of that name then
# raised "Redeclaration of routine" -- which is what roast/S04-statements/given.t
# hit, EVALing a fresh `sub test-given` per subtest while an earlier subtest's
# `my sub test-given` was still registered.

plan 6;

sub run(&b) { b() }

my @log;

run {
    my sub tester($x) { @log.push: "outer $x" }
    tester(1);
}

lives-ok {
    run {
        my sub produce($n) {
            EVAL "sub tester(\$x) \{ \@log.push: \"eval$n \$x\" }";
        }
        for 1, 2 -> $n {
            my &t = produce $n;
            t(9);
        }
    }
}, 'an EVALd sub shadows a routine left registered by a callable block';

is-deeply @log, ['outer 1', 'eval1 9', 'eval2 9'],
    'and each EVAL produced its own routine';

# The same shape with the outer routine at file scope.
sub top-level($a, $b) { "$a$b" }
is top-level(1, 2), '12', 'the file-scope routine works before the EVAL';
my $evaled = EVAL 'sub top-level($x) { "e$x" }';
is $evaled(7), 'e7', 'an EVALd sub may shadow a file-scope routine of the same name';
is top-level(1, 2), '12', "and the outer routine is unaffected afterwards";

# A name declared *inside* the same EVAL still conflicts -- the exemption must
# not turn into a blanket "anything goes inside EVAL".
throws-like { EVAL 'sub dup-in-eval() { 1 }; sub dup-in-eval() { 2 }' },
    Exception,
    'two declarations of one name inside a single EVAL still collide';
