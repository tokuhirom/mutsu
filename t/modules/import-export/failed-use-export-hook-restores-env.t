use Test;

# A `use` whose module's `sub EXPORT` dies (a rejected import list) must leave
# the importer's environment as it found it. The hook runs in the module's own
# env; when it died the importer kept running in that env, so a closure's
# captured `@`/`%` variables (read by name) came back as `Nil` afterwards.
plan 6;

use lib 't/lib';
use FailingExportHook <&fehook-answer>;

sub run(&blk) { blk() }

sub with-closure(%h, @a, $s) {
    my $seen;
    run({
        try { EVAL "use FailingExportHook <Nope>" };
        $seen = [%h.raku, @a.raku, $s.raku];
    });
    $seen
}
is-deeply with-closure(%(k => 1), [1, 2], "S"),
    ['{:k(1)}', '[1, 2]', '"S"'],
    'captured %, @ and $ survive a failed use in a closure';

sub two-failures(%h, @a) {
    my $seen;
    run({
        try { EVAL "use FailingExportHook <Nope>" };
        try { EVAL "use FailingExportHook <Nada>" };
        $seen = [%h.raku, @a.raku];
    });
    $seen
}
is-deeply two-failures(%(k => 1), [1, 2]), ['{:k(1)}', '[1, 2]'],
    'and survive two failed uses in a row';

sub failure-then-success(%h) {
    my $seen;
    run({
        try { EVAL "use FailingExportHook <Nope>" };
        EVAL "use FailingExportHook <&fehook-answer>";
        $seen = %h.raku;
    });
    $seen
}
is failure-then-success(%(k => 1)), '{:k(1)}', 'a successful use after the failure still works';

sub message-kept {
    my $m;
    try { EVAL "use FailingExportHook <Nope>"; CATCH { default { $m = .message } } }
    $m
}
is message-kept(), 'not exported: Nope', 'the hook\'s message reaches the CATCH';

is fehook-answer(), 42, 'the earlier successful import is intact';

sub hash-after-failed-use-in-closure(%h) {
    my @r;
    run({
        try { EVAL "use FailingExportHook <Nope>" };
        @r.push: %h.raku;
    });
    @r[0]
}
is hash-after-failed-use-in-closure(%(k => 1)), '{:k(1)}', 'a captured % is intact (closure, one failed use)';
