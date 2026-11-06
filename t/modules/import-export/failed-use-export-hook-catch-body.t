use Test;

# A `CATCH` the importer installed runs at the throw site, i.e. inside the
# module's `sub EXPORT` hook when a `use` is rejected. It must see (and write)
# the importer's variables, not the module's env (#16968).
plan 5;

use lib 't/lib';
use FailingExportHook <&fehook-answer>;

sub run(&blk) { blk() }

sub write-local {
    my $seen = "unset";
    run({
        try {
            EVAL "use FailingExportHook <Nope>";
            CATCH { default { $seen = "caught" } }
        }
    });
    $seen
}
is write-local(), 'caught', 'a CATCH writes a closure-captured scalar';

sub read-captures(%h, @a, $s) {
    my $seen;
    run({
        try {
            EVAL "use FailingExportHook <Nope>";
            CATCH { default { $seen = [%h.raku, @a.raku, $s.raku, .message] } }
        }
    });
    $seen
}
is-deeply read-captures(%(k => 1), [1, 2], "S"),
    ['{:k(1)}', '[1, 2]', '"S"', 'not exported: Nope'],
    'a CATCH reads captured %, @ and $';

sub loop-over-hash(%matcher) {
    my @r;
    run({
        try {
            EVAL "use FailingExportHook <Nope>";
            CATCH { default { for %matcher.sort -> $p { @r.push: "{$p.key}={$p.value}" } } }
        }
    });
    @r
}
is-deeply loop-over-hash(%(message => "x", a => 1)), ['a=1', 'message=x'],
    'a CATCH iterates a captured hash';

throws-like 'use FailingExportHook <Nope>', Exception, message => /'not exported'/;
throws-like { EVAL 'use FailingExportHook <Nope>' }, Exception, message => /'not exported'/;
