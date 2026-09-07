use v6;
use Test;

# The `@`/`%` half of ADR-0055's capture dichotomy (the scalar half is pinned by
# `t/closure-capture-cell-dichotomy.t`).
#
# A container lexical is reference-shared, so the captured VALUE is live: a
# post-capture `@a.push` is visible to the closure. That made the container half
# look immune to the hijack, and `needs_cell_unvouched_locals` shipped with an
# explicit `@`/`%` skip. It is not immune, because the hijack is not a staleness
# defect: the closure-call merge resolves the NAME, and its don't-overwrite
# default hands a same-named container in whatever frame happens to be calling
# the win. A plain `Array` capture carries nothing that makes the merge prefer
# the closure's own binding -- `ContainerRef` is that "nothing", so an escaping
# container capture the creating frame cannot vouch for takes the cell too.
#
# Both failure directions are pinned throughout:
#   * HIJACK    -- a same-named container in the calling frame wins, and
#   * STALENESS -- the creator's post-capture mutation is invisible.

plan 23;

# ---------------------------------------------------------------------------
# 1-2. The headline defect, `@` and `%`. `@a.push(3)` is load-bearing: an
# in-place container write is what `own_container_writes` refuses to vouch for,
# so the capture is neither authoritative nor (before this fix) celled.
# ---------------------------------------------------------------------------
{
    my @a = 1, 2;
    @a.push(3);
    my $f = -> { @a.elems };
    sub collide-at() { my @a = 9; my $g = { @a.elems }; $g.(); $f.() }
    is collide-at(), 3,
        'an unvouched @ capture keeps its own binding under a same-named caller array';
}
{
    my %h = a => 1, b => 2;
    %h<c> = 3;
    my $f = -> { %h.elems };
    sub collide-pct() { my %h = z => 9; my $g = { %h.elems }; $g.(); $f.() }
    is collide-pct(), 3,
        'an unvouched % capture keeps its own binding under a same-named caller hash';
}

# ---------------------------------------------------------------------------
# 3-4. Slot-resident variant. The scalar half of this family needs a decoy
# closure in the caller (`my $g = { $b }`) to force the colliding lexical out of
# a local slot and into `env`; a container declaration reaches `env`
# unconditionally, so the plain form collides on its own.
# ---------------------------------------------------------------------------
{
    my @a = 1, 2;
    @a.push(3);
    my $f = -> { @a.elems };
    sub collide-at-slot() { my @a = 9; $f.() }
    is collide-at-slot(), 3, '... with no decoy closure in the caller (@)';
}
{
    my %h = a => 1, b => 2;
    %h<c> = 3;
    my $f = -> { %h.elems };
    sub collide-pct-slot() { my %h = z => 9; $f.() }
    is collide-pct-slot(), 3, '... with no decoy closure in the caller (%)';
}

# ---------------------------------------------------------------------------
# 5-8. The STALENESS direction. These passed before the cell existed, and the
# cell must not trade the hijack fix for a frozen snapshot: every kind of
# post-capture write -- in-place mutation AND whole-container reassignment --
# stays visible to the closure.
# ---------------------------------------------------------------------------
{
    my @a = 1, 2;
    my $f = -> { @a.elems };
    @a.push(3);
    is $f.(), 3, 'a post-capture push is visible to the closure';
}
{
    my %h = a => 1;
    my $f = -> { %h.elems };
    %h<b> = 2;
    is $f.(), 2, 'a post-capture hash element write is visible to the closure';
}
{
    my @a = 1, 2;
    my $f = -> { @a.elems };
    @a = 7, 8, 9, 10;
    is $f.(), 4, 'a post-capture whole-array reassignment is visible to the closure';
}
{
    my %h = a => 1;
    my $f = -> { %h.elems };
    %h = x => 1, y => 2, z => 3;
    is $f.(), 3, 'a post-capture whole-hash reassignment is visible to the closure';
}

# ---------------------------------------------------------------------------
# 9-10. The staleness direction once the creating frame is GONE: a returned
# closure must still report the container's final contents, so the cell is not
# a snapshot taken at capture time.
# ---------------------------------------------------------------------------
{
    sub mk-push() { my @a = 1, 2; my $f = -> { @a.elems }; @a.push(3); return $f }
    is mk-push().(), 3, 'a push after capture but before return survives the frame';
}
{
    sub mk-reassign() { my @a = 1, 2; my $f = -> { @a.elems }; @a = 7, 8, 9; return $f }
    is mk-reassign().(), 3, 'a reassignment after capture but before return survives the frame';
}

# ---------------------------------------------------------------------------
# 11. The closure's own mutation must reach ITS array, not the caller's. This is
# the hijack's most damaging form: before the cell, `$f.()` pushed onto the
# calling frame's unrelated `@a`.
# ---------------------------------------------------------------------------
{
    my @a = 1, 2;
    @a.push(3);
    my $f = -> { @a.push(4); @a.elems };
    sub collide-write() { my @a = 9; $f.() }
    my $inner = collide-write();
    is "$inner/{@a.elems}", '4/4',
        'a mutating capture writes through to its own array, not the caller\'s';
}

# ---------------------------------------------------------------------------
# 12-13. The other write shape the vouch refuses, and the typed containers,
# which take the cell like everything else. An ELEMENT-typed `@`/`%` used to be
# refused it -- it shared one check with the CONTAINER type traits
# (`my %h is BagHash`), whose declaration store really does have to keep flowing
# through the assignment chokepoint that coerces the QuantHash. But those never
# reached that check at all (`is BagHash` is invisible to `var_type_constraint`;
# `compute_free_vars` carries a separate `ApplyVarTrait` name scan for them), so
# the refusal only ever cost the element-constraint case, which ADR-0042 made a
# property of the container and which therefore survives the cell. 13 pins BOTH
# halves: the binding, and the type behaviour it must not cost.
# ---------------------------------------------------------------------------
{
    my @a = 1, 2;
    @a[2] = 3;
    my $f = -> { @a.elems };
    sub collide-elem() { my @a = 9; $f.() }
    is collide-elem(), 3, 'an element-assign (not push) capture keeps its own binding';
}
{
    my Int @a = 1, 2;
    @a.push(3);
    my $f = -> { @a.elems };
    sub collide-typed() { my Int @a = 9; $f.() }
    my $err = 'no-error';
    try {
        @a.push("not an Int");
        CATCH { default { $err = 'died' } }
    }
    # A distinct name on purpose: the `is <Type>` exclusion is by NAME across
    # the whole frame (same-named `my` locals share one slot), so calling this
    # `%h` would opt tests 2 and 4 out of the cell as well.
    my %bag is BagHash = a => 1, b => 0, c => 2;
    is "{collide-typed()}/{@a.WHAT.^name}/$err/{%bag.elems}", '3/Array[Int]/died/2',
        'a typed container takes the cell, keeping its element check and its container type';
}

# ---------------------------------------------------------------------------
# 14-16. The invocation paths. They differ in WHICH merge runs, and the capture
# is the same binding in all of them.
# ---------------------------------------------------------------------------
{
    my @a = 1, 2;
    @a.push(3);
    my $f = -> { @a.elems };
    sub deeper() { $f.() }
    sub collide-two-frames() { my @a = 9; deeper() }
    is collide-two-frames(), 3, 'the collision may be two frames up';
}
{
    my @a = 1, 2;
    @a.push(3);
    my $f = -> $x { @a.elems };
    sub collide-map() { my @a = 9; (1,).map($f).join(',') }
    is collide-map(), '3', '... and through .map($f)';
}
{
    my @a = 1, 2;
    @a.push(3);
    my $f = -> { @a.elems };
    sub collide-loop() { for 1..1 { my @a = 9; return $f.() } }
    is collide-loop(), 3, '... and from inside a loop body that declares the name';
}

# ---------------------------------------------------------------------------
# 17-18. Per-instance and per-iteration freshness. Each closure must keep the
# container its own invocation/iteration created, both on its own and under a
# colliding caller -- a cell that leaked across invocations would collapse them.
# ---------------------------------------------------------------------------
{
    sub mk($n) { my @a; @a.push($n); return -> { @a.join(',') } }
    my $f1 = mk(1);
    my $f2 = mk(2);
    sub collide-instances() { my @a = 99; ($f1.(), $f2.()).join(' ') }
    is collide-instances(), '1 2',
        'two closures from one factory keep their own arrays under a colliding caller';
}
{
    my @cb;
    for 1..3 -> $i {
        my @a;
        @a.push($i);
        @cb.push(-> { @a.join(',') });
    }
    is @cb.map({ $_.() }).join('|'), '1|2|3',
        'a per-iteration container binding stays distinct per closure';
}

# ---------------------------------------------------------------------------
# 19. Two sibling closures over ONE container share it: the cell must be the
# container's identity, not a per-closure copy -- so a push through one is
# visible to the other, even under a colliding caller.
# ---------------------------------------------------------------------------
{
    sub mk-pair() {
        my @a = 1, 2;
        return (-> { @a.elems }, -> { @a.push(9) });
    }
    my ($get, $push) = mk-pair();
    sub collide-siblings() { my @a = 5, 5, 5, 5, 5; $push.(); $get.() }
    is collide-siblings(), 3,
        'sibling closures share one container cell, and neither sees the caller\'s array';
}

# ---------------------------------------------------------------------------
# 20. An `our` container, whose lexical alias is an own local like any other.
# ---------------------------------------------------------------------------
{
    our @shared = 1, 2;
    @shared.push(3);
    my $f = -> { @shared.elems };
    sub collide-our() { my @shared = 9; $f.() }
    is collide-our(), 3, 'an `our` container capture keeps its own binding';
}

# ---------------------------------------------------------------------------
# 21-22. Two SIBLING blocks that each declare the same container name share one
# local slot, so the second block's declaration finds the first block's cell
# still sitting in it. The cell must then be re-published into `env` rather than
# short-circuited on (the scalar lane's `is_container_ref` early return): with
# the skip, the escaping closure captured the first block's leftover plain array
# from `env`, pushed into that, and the owner — reading the slot — saw an empty
# container, with the writes surfacing one block late.
# ---------------------------------------------------------------------------
{
    class Sink {
        has $.cb is rw;
        method hop() { self.hop2 }
        method hop2() { for <p q> { $!cb.($_) } }
        method go(:$out) { self.cb = $out; self.hop }
    }
    my @first;
    for 1..1 {
        my @d;
        my $s = Sink.new;
        $s.cb = { @d.push: $_ };
        $s.hop;
        @first.push([@d]);
    }
    for 1..2 {
        my @d;
        my $s = Sink.new;
        $s.go(out => { @d.push: $_ });
        @first.push([@d]);
    }
    is @first.map({ .join(',') }).join('|'), 'p,q|p,q|p,q',
        'a sibling block that redeclares the name gets its own writes, not the previous block\'s';
}
{
    my @outer;
    for 1..1 { my %h; my $s = -> $k { %h{$k} = 1 }; $s.('a'); @outer.push(%h.elems) }
    for 1..2 { my %h; my $s = -> $k { %h{$k} = 1 }; $s.('b'); $s.('c'); @outer.push(%h.elems) }
    is @outer.join(','), '1,2,2', '... and the same for a hash';
}

# ---------------------------------------------------------------------------
# 23. `squish(:with)` runs its callbacks eagerly and reverts their side effects
# when the lazy iterator is asked for. The revert has to reach a celled `@`
# accumulator through its CELL: replacing the env binding would leave the eager
# pass's pushes standing (and detach every alias from the name), so the reset
# below would silently be undone.
# ---------------------------------------------------------------------------
{
    my @with;
    my $with = { @with.push: "$^a $^b"; $^a + 1 == $^b };
    @with = ();
    my $i := (1, 2, 3, 2, 1, 0).squish(:$with).iterator;
    $i.pull-one;
    is @with.elems, 0, 'a squish :with accumulator reset before the iterator stays reset';
}

done-testing;
