use Test;

# ADR-0038: `Seq.cache` must return a `List`, never something that still
# binds `Seq:D` -- rakudo relies on this as the termination condition for
# `Test.rakumod`'s `multi is-deeply(Seq:D, ...)` narrowing (see the ADR §1.3);
# when `.cache` fails to narrow, that multi re-dispatches to itself forever
# and the real stack overflows (SIGABRT). This file pins the underlying
# contract directly rather than through `is-deeply`'s recursion, so a
# regression shows up as a normal test failure instead of a crashed process.
#
# The matrix below is the ADR §1.4 table, re-measured against `raku`
# (`tmp/matrix-check.raku` in the ticket) rather than hand-written: every row
# answers `.cache.^name eq 'List'`, `!(.cache ~~ Seq:D)`, `.cache ~~ List:D`.

plan *;

sub check-cache-is-list($label, $val) {
    my $cached = $val.cache;
    is $cached.^name, 'List', "$label: .cache.^name is List";
    nok $cached ~~ Seq:D, "$label: .cache does not bind Seq:D";
    ok $cached ~~ List:D, "$label: .cache binds List:D";
}

# --- already-reified SeqBody: worked before this ADR, pinned as a baseline ---
check-cache-is-list('(1,2,3).Seq', (1, 2, 3).Seq);
check-cache-is-list('<a b c>.tail(*+10)', <a b c>.tail(*+10));

# --- lazy pipe / gather (LazyList, not cat-pull): also worked before ---
check-cache-is-list('map pipe', (1, 2, 3).map({ $_ }));
check-cache-is-list('gather', gather { take 1; take 2; take 3 });

# --- facet A: a deferred SeqBody (Seq.new($iterator)) -- ADR-0038 phase 3 ---
sub make-iterator-seq() {
    Seq.new(class :: does Iterator {
        has @!stuff = <a b c>;
        method pull-one { @!stuff and return @!stuff.shift; IterationEnd }
    }.new);
}
check-cache-is-list('Seq.new($iterator)', make-iterator-seq());

# The ORIGINAL Seq value must keep reporting Seq after `.cache` is taken from
# it -- the view is a property of the returned handle, not the shared body
# (measured against raku: `$s.^name` stays `Seq` even after `$s.cache`).
{
    my $s = make-iterator-seq();
    my $c = $s.cache;
    is $s.^name, 'Seq', 'the original Seq handle still reports Seq after .cache';
    is $c.^name, 'List', 'the .cache handle reports List';
}

# `.cache` on a genuinely infinite deferred Seq must NOT pull anything --
# `.cache.^name` answers `List` before any element is produced.
{
    my $pulls = 0;
    my $s = Seq.new(class :: does Iterator {
        has $!n = 0;
        method pull-one { $pulls++; $!n++ }
    }.new);
    is $s.cache.^name, 'List', '.cache of an infinite deferred Seq reports List without pulling';
    is $pulls, 0, '.cache did not pull any elements';
}

# --- facet A: IO::Handle.lines / .words (deferred SeqBody, IoLines source) ---
{
    my $p = $*TMPDIR.add("mutsu-seq-cache-returns-list-{$*PID}");
    $p.spurt("a\nb\nc\n");
    check-cache-is-list('IO::Handle.lines', $p.IO.open(:r).lines);
    check-cache-is-list('IO::Handle.words', $p.IO.open(:r).words);

    # --- facet B: IO::CatHandle.lines / .handles (cat-pull LazyList) ---
    check-cache-is-list('IO::CatHandle.lines', IO::CatHandle.new($p).lines);
    check-cache-is-list('IO::CatHandle.handles', IO::CatHandle.new($p).handles);

    $p.unlink;
}

done-testing;
