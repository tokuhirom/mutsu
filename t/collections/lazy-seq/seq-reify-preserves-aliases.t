use Test;

# ADR-0034 §1.3's measured probe: `.cache` reifies a deferred Seq's elements
# INTO the Seq's own body (`Arc<SeqBody>`), so every alias of the same value
# — a second lexical bound to it, or a caller's copy passed one frame away —
# sees the reified elements afterward. A name-keyed writeback (the pre-ADR
# band-aid) cannot reach either case, because both are properties of the
# *value*, not of any particular name.
#
# Row numbers below match the ADR's §1.3 table (`tmp/probe2.raku`).

plan *;

my $file = $*TMPDIR.add("seq-reify-aliases-{$*PID}.txt");
$file.spurt("A\nB\nC\n");
END { $file.unlink if $file.e; }

sub fresh-seq() {
    $file.open(:r).lines;
}

# Row 1: `.cache` then `.List` on the SAME name still works (the baseline —
# this passed even before ADR-0034, via the name-keyed writeback band-aid).
{
    my $a = fresh-seq();
    $a.cache;
    is-deeply $a.List, ("A", "B", "C").List, 'row 1: .cache then .List on the same name';
}

# Row 2: `.cache` called one call frame away (through a sub taking the Seq as
# a parameter) must still reify the CALLER's binding — a name-keyed writeback
# cannot reach across the frame boundary, but a value-level reify can.
{
    sub reify-it($s) { $s.cache }
    my $b = fresh-seq();
    reify-it($b);
    is-deeply $b.List, ("A", "B", "C").List,
        "row 2: .cache through a sub parameter reifies the caller's Seq";
}

# Row 3: two lexicals aliasing the SAME Seq value — `.cache` through one name
# must be visible through the other, since they share one `Arc<SeqBody>`.
{
    my $d = fresh-seq();
    my $e = $d;
    $d.cache;
    is-deeply $e.List, ("A", "B", "C").List,
        'row 3: .cache through one alias is visible through a second alias';
}

# Row 5: a user-defined Iterator's `pull-one` runs exactly once even across
# two non-consuming touches (`.Str` twice) — the second `.Str` must reuse the
# first one's reified elements instead of re-pulling (which would see
# IterationEnd immediately and silently render an empty/short string).
{
    class CountingIterator does Iterator {
        has $.n = 0;
        method pull-one() {
            return IterationEnd if $!n >= 3;
            my $v = $!n;
            $!n++;
            $v;
        }
    }
    my $s = Seq.new(CountingIterator.new);
    my $first = $s.Str;
    my $second = $s.Str;
    is $first, '0 1 2', 'row 5: first .Str on a deferred user-Iterator Seq';
    is $second, '0 1 2',
        'row 5: second .Str reuses the reified elements (no silent re-pull)';
}

done-testing;
