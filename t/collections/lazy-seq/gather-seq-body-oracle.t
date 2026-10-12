use v6;
use Test;

# ADR-11678 §5 step 0: Rakudo's behaviour for a plain `gather`, pinned before
# the gather became a `SeqSource::Gather` adapter over its `LazyList`. Every
# expectation below is Rakudo v2026.09's output: rows 1-17 of the ADR's §1.2
# table (fixed by the adapter), and the behaviours that already matched before
# it and must stay so.

plan 60;

# --- Itemization (§1.2 rows 1-7) ---------------------------------------------

{
    my $g = gather { take 2 };
    is ("a", $g).raku, '("a", $((2,).Seq))', 'row 1: ("a", $g).raku';
}
{
    my $g = gather { take 2 };
    is $g.raku, '$((2,).Seq)', 'row 2: $g.raku';
}
{
    my %h = a => (gather { take 5 });
    is (%h<a>:kv).raku, '("a", $((5,).Seq))', 'row 3: (%h<a>:kv).raku';
}
{
    my $g = gather { take 1 };
    is (:k($g)).raku, ':k($((1,).Seq))', 'row 4: (:k($g)).raku';
}
{
    my $g = gather { take 1 };
    is [$g].raku, '[(1,).Seq,]', 'row 5: [$g].raku';
}
{
    my $g = gather { take 1; take 2 };
    is ($g, 2).flat.elems, 2, 'row 6: ($g, 2).flat.elems';
}
{
    my $g = gather { take 1; take 2 };
    my @a = $g;
    is @a.elems, 1, 'row 7: my @a = $g';
}

# --- Identity (rows 8-10) ----------------------------------------------------

{
    my $g = gather { take 1; take 2 };
    ok $g === $g, 'row 8: $g === $g';
}
{
    my $g := gather { take 1; take 2 };
    ok $g === $g, 'row 8: $g === $g (bound)';
}
{
    my $g = gather { take 1; take 2 };
    my $h = $g;
    ok $g === $h, 'row 9: my $h = $g; $g === $h';
}
{
    my $g = gather { take 1; take 2 };
    ok $g.WHICH eq $g.WHICH, 'row 10: $g.WHICH eq $g.WHICH';
}

# --- Consumption (rows 11-14) ------------------------------------------------

{
    my $g = gather { take 1; take 2 };
    $g.head(1);
    throws-like { $g.List }, X::Seq::Consumed, 'row 11: .head(1) then .List';
}
{
    my $g = gather { take 1; take 2 };
    $g.List;
    throws-like { $g.List }, X::Seq::Consumed, 'row 12: .List twice';
}
{
    my $g = gather { take 1; take 2 };
    my $i = $g.iterator;
    $i.pull-one;
    throws-like { $g.List }, X::Seq::Consumed, 'row 13: .iterator then .List';
}
{
    my $g = gather { take 1; take 2 };
    my $h = $g;
    $h.List;
    throws-like { $g.List }, X::Seq::Consumed, 'row 14: alias .List then .List';
}

# --- Resume after a prefix (rows 15-17, wrong answers) -----------------------

# Run as child programs so the output is exactly what `say` prints (the
# divergence shows in the pure `say` renderer, not in `.Str`).
sub out-of(Str $code) {
    my $proc = run $*EXECUTABLE, '-e', $code, :out, :err;
    my $out = $proc.out.slurp(:close);
    $proc.err.slurp(:close);
    $out.lines.join(' ')
}

is out-of('my $g = gather { take 1; take 2 }; my $f = $g[0]; say $g.List'),
    '(1 2)', 'row 15: $g[0]; $g.List';
is out-of('my $g = gather { take 1; take 2 }; my $b = ?$g; say $g.List'),
    '(1 2)', 'row 16: ?$g; $g.List';
is out-of('my $g = gather { for 1..3 { say "t$_"; take $_ } }; say $g[0]; my $h = $g; say $h.List; say $g.List'),
    't1 1 t2 t3 (1 2 3) (1 2 3)', 'row 17: an alias resumes the coroutine once';

# --- Already matching (§1.2): must stay so ----------------------------------

{
    my $runs = 0;
    my $l = (1, gather { for 1..* { $runs++; take $_ } });
    my $p = (:a(gather { for 1..* { $runs++; take $_ } }), 5);
    my %h = a => (gather { for 1..* { $runs++; take $_ } });
    is $runs, 0, 'an infinite gather in a list, Pair or Hash does not run';
    is $l[1][^3], (1, 2, 3), 'list element answers [^3]';
    is $p[0].value[^2], (1, 2), 'Pair value answers [^2]';
    is %h<a>[^2], (1, 2), 'Hash value answers [^2]';
}
{
    my $runs = 0;
    my $inf = gather { for 1..* { $runs++; take $_ } };
    is $inf.head(3), (1, 2, 3), 'infinite .head(3)';
    is $runs, 3, '.head(3) runs the body three times';
}
{
    my $inf = gather { for 1..* { take $_ } };
    is $inf[^3], (1, 2, 3), 'infinite [^3]';
    is $inf[5], 6, 'then [5]';
}
{
    my @log;
    for gather { for 1..* { @log.push: "t$_"; take $_ } } -> $x {
        @log.push: "b$x";
        last if $x >= 2;
    }
    is @log.join(' '), 't1 b1 t2 b2', 'for over a gather interleaves body and loop';
}
{
    my @log;
    my $g = gather { for 1..* { @log.push: "t$_"; take $_ } };
    for $g<> -> $x {
        @log.push: "b$x";
        last if $x >= 2;
    }
    is @log.join(' '), 't1 b1 t2 b2', 'for over $g<> interleaves body and loop';
}
{
    my $runs = 0;
    my $inf = gather { for 1..* { $runs++; take $_ } };
    my $it = $inf.iterator;
    is $it.pull-one, 1, 'first pull-one';
    is $it.pull-one, 2, 'second pull-one';
    is $runs, 2, 'two pull-one calls run the body twice';
}
{
    my $g = gather { take 1; take 2 };
    $g.cache;
    is $g.List, (1, 2), '.cache then .List';
    is $g.List, (1, 2), '.cache then .List again';
}
{
    my $g = gather { take 1; take 2 };
    is $g.elems, 2, '.elems';
    is $g.List, (1, 2), '.elems then .List';
}
{
    my $g = gather { take 1; take 2 };
    is @$g.elems, 2, '@$g';
    is @$g.elems, 2, '@$g again';
}
{
    my $g = gather { take 1; take 2 };
    ok $g eqv $g, '$g eqv $g';
}
{
    my $r = try { (gather { take 1; die "boom" }).eager };
    nok $r.defined, 'die in the body propagates to try';
    like $!.message, /boom/, 'the exception is the body\'s';
}
{
    my @a = gather { take 1; take 2 };
    is @a.raku, '[1, 2]', 'my @a = gather {...}';
}
{
    my $g = gather { take 1 };
    is $g.^name, 'Seq', '.^name is Seq';
    nok $g.is-lazy, 'a plain gather is not lazy';
}
{
    my $l = lazy gather { take 1 };
    is $l.raku, '$((1).lazy.Seq)', 'a $-held lazy gather renders itemized';
}

# --- Slips, nesting, control flow --------------------------------------------

{
    my $g = gather { take 1; take |(2, 3); take 4 };
    is $g.List, (1, 2, 3, 4), 'take of a Slip flattens into the gather';
}
{
    my $g = gather { take 1; take (2, 3); take 4 };
    is $g.elems, 3, 'take of a List is one element';
}
{
    my $g = gather { take 1; take gather { take 2; take 3 } };
    my @l = $g.List;
    is @l.elems, 2, 'take of a nested gather is one element';
    is @l[1].List, (2, 3), 'the nested gather yields its elements';
}
{
    # #13003: a `return` in the body returns from the routine that wrote the
    # gather while that routine is still running, whatever forces the gather.
    sub f { my $g = gather { take 1; return 42; take 2 }; $g.eager; 99 }
    is (try f()), 42, 'return in the body returns from the routine that wrote it';
    sub f2 { my $g = gather { take 1; return 42; take 2 }; my $s = ~$g; 99 }
    is (try f2()), 42, '... when the gather is stringified';
    sub f3 { my @g = gather { take 1; return 42; take 2 }; 99 }
    is (try f3()), 42, '... when the gather is assigned to an array';
    sub f4 { (gather { take 1; return 42; take 2 }).eager; 99 }
    is (try f4()), 42, '... when an unstored gather is forced';
}
{
    my $g = gather { for 1..5 { next if $_ == 2; last if $_ == 4; take $_ } };
    is $g.List, (1, 3), 'next/last in a for inside the body';
}
{
    my $g = gather { take 1; take 2; take 3 };
    is $g.head(2), (1, 2), 'consuming .head(2)';
}
{
    my $g = gather { take 5; take 6 };
    is $g.first, 5, '.first';
}
{
    my $g = gather { take 1; take 2 };
    my $c = $g.cache;
    is $c, (1, 2), '.cache value';
    is $g.cache, (1, 2), '.cache again';
}

# --- GC: an unforced self-referencing gather -------------------------------

{
    # ADR-11678 §7 item 4: the adapter must reclaim at least as many of these
    # cycles (object -> gather -> captured env -> object) as before. Both
    # mutsu (before the adapter) and Rakudo report 399 cyclic and at least
    # 399 acyclic reclaims (the last loop iteration's object may still be
    # live at the collection).
    my $code = q:to/END/;
        my $cyclic = 0;
        class Holder { has $.g is rw; submethod DESTROY { $cyclic++ } }
        for ^400 {
            my $h = Holder.new;
            $h.g = gather { take $h; take 1 };
        }
        $*VM.request-garbage-collection;
        my $acyclic = 0;
        class Plain { has $.g is rw; submethod DESTROY { $acyclic++ } }
        for ^400 { my $h = Plain.new; $h.g = gather { take 1 } }
        $*VM.request-garbage-collection;
        say "$cyclic $acyclic";
        END
    my $proc = run $*EXECUTABLE, '-e', $code, :out, :err;
    my ($cyclic, $acyclic) = $proc.out.slurp(:close).words;
    $proc.err.slurp(:close);
    is $proc.exitcode, 0, 'the GC child exits cleanly';
    ok $cyclic >= 399, 'unforced self-referencing gathers are reclaimed';
    ok $acyclic >= 399, 'unforced plain gathers are reclaimed';
}
