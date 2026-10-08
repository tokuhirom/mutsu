use Test;

# ADR-0034 §1.4's measured raku-vs-mutsu consumption matrix, pinned as a
# regression test: a deferred `IO::Handle.lines` Seq (SeqSource::IoLines) is
# built fresh for each row, one method is called on it, and we check whether
# the Seq is still usable afterward (`.List` on the SAME Seq value).
#
# Row 1 ("keeps"): these methods must NOT consume the Seq — they either never
# touch the source (WHAT/WHICH/is-lazy/defined/DEFINITE) or route through
# rakudo's `.cache` primitive (Str/gist/raku/Bool/so/elems/cache itself),
# which reifies into the Seq rather than stealing its iterator.
#
# Row 2 ("consumes"): these route through rakudo's `.iterator`/`.list`
# primitive, which hands the source away once — a second read throws
# X::Seq::Consumed.
#
# See docs/adr/0034-seq-reification-is-in-place-and-distinct-from-consumption.md
# §1.4 for the full oracle this table is measured against.

plan *;

my $file = $*TMPDIR.add("seq-consumption-matrix-{$*PID}.txt");
$file.spurt("1\n2\n3\n");
END { $file.unlink if $file.e; }

sub fresh-seq() {
    $file.open(:r).lines;
}

my @keeps = <Str gist raku Bool so elems cache WHAT WHICH defined DEFINITE is-lazy>;
for @keeps -> $method {
    my $s = fresh-seq();
    try { $s."$method"() };
    lives-ok { $s.List }, "$method keeps the Seq reusable (.List still works after)";
}

my @consumes = <
    List Array eager flat sort reverse join head tail first sum min max
    kv pairs Set Bag hyper race lazy sink
>;
for @consumes -> $method {
    my $s = fresh-seq();
    try { $s."$method"() };
    throws-like { $s.List }, X::Seq::Consumed, "$method consumes the Seq";
}

# `.iterator` consumes too (rakudo's other primitive that steals the source;
# this was the specific gap ADR-0034's headline repro measured mutsu getting
# backwards — see the ADR's row 3, ".iterator: raku consumes / mutsu (pre-fix)
# keeps").
{
    my $s = fresh-seq();
    try { $s.iterator };
    throws-like { $s.List }, X::Seq::Consumed, '.iterator consumes the Seq';
}

# `.list` on a genuinely deferred source (this test file's `fresh-seq()`)
# consumes, same as `.List` above -- the parser ambiguity's compromise
# (`reify_or_consume_seq_target`'s `"list"` branch in
# src/vm/vm_helpers_lazy.rs) only special-cases an already-`Reified` body.
{
    my $s = fresh-seq();
    $s.list;
    throws-like { $s.List }, X::Seq::Consumed, '.list consumes a deferred source';
}

# An EXPLICIT `.list` call on an ALREADY-`Reified` body (a `.map`/`.grep`
# result) consumes, as in raku, while the `@$s` / `@($s)` contextualizer over
# the same Seq keeps re-reading it (#9930; the compiler marks the explicit call
# with `OpCode::ConsumeReifiedSeq`, the parser tags the contextualizer
# `sugar: true`). Pinned against `t/seq-array-context-reiterate.t`.
{
    my $s = (1, 2, 3).map({ $_ });
    $s.list;
    throws-like { $s.List }, X::Seq::Consumed,
        'explicit .list on a Reified body consumes it';
}
{
    my $s = (1, 2, 3).map({ $_ });
    $s.list;
    throws-like { $s.list }, X::Seq::Consumed,
        'a second explicit .list on the same Seq throws';
}
{
    my $s = (1, 2, 3).map({ $_ });
    my @a = @$s;
    my @b = @$s;
    is-deeply (@a, @b), ([1, 2, 3], [1, 2, 3]), '@$s re-reads a Reified body';
    is @($s).elems, 3, '@($s) re-reads it too';
    lives-ok { $s.List }, '@$s / @($s) did not consume the Seq';
}

done-testing;
