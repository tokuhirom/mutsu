use Test;

# ADR-0036 slice 3, the `.pairs` half.
#
# `.pairs` hands an element OUT, so in raku the Pair it yields carries the
# element's own `Scalar` container -- reading through the pair sees later
# writes to the source, and writing through it writes the source back.
# `t/subscript-pair-element-container.t` pins the §1.3 rows themselves; this
# file pins the CONSUMER side, which is what kept `.pairs` unrouted from
# 2026-08-27 until now: a Pair whose value is a cell reaches code that reads
# that value structurally (a Bag weight, a Hash construction, a type test),
# and every such read must decontainerize.
#
# Cross-checked against `raku` line by line.

plan 37;

# --- the container itself ---------------------------------------------------

{
    my @a = <A B>;
    my $p = @a.pairs[0];
    @a[0] = "Q";
    is $p.value, "Q", 'array .pairs value tracks a later element write';
    is @a.pairs[0].value.VAR.^name, 'Scalar', 'array .pairs value is a Scalar container';
}

{
    my %h = a => 1;
    my $p = %h.pairs[0];
    %h<a> = 7;
    is $p.value, 7, 'hash .pairs value tracks a later element write';
    is %h.pairs[0].value.VAR.^name, 'Scalar', 'hash .pairs value is a Scalar container';
}

{
    my @a = <A B>;
    my @sibling = <A B>;   # the equal-valued copy that defeated the env scan
    for @a.pairs -> $p { $p.value = "y" }
    is-deeply @a, ["y", "y"], 'for @a.pairs writes through, sibling copy untouched';
    is-deeply @sibling, ["A", "B"], 'the sibling array is not written';
}

{
    my %h = a => 1, b => 2;
    for %h.pairs -> $p { $p.value = $p.value * 10 }
    is-deeply %h, {a => 10, b => 20}, 'for %h.pairs writes through';
}

# The promotion is in-place, and must stay invisible to value context.
{
    my @a = <A B>;
    @a.pairs.sink;
    is @a.elems, 2, '.pairs does not grow the source array';
    is @a.raku, '["A", "B"]', '.pairs leaves .raku unchanged';
    is @a[0].WHAT.gist, '(Str)', 'an element still reads back as its own type';
    is @a.pairs[0].value.WHAT.gist, '(Str)', 'the pair value reports the element type';
}

# --- immutable sources keep the snapshot producer --------------------------

{
    my $l = (1, 2);
    dies-ok { $l.pairs[0].value = 3 }, 'a List .pairs value is immutable';
    is-deeply $l.pairs.List, (0 => 1, 1 => 2), 'and still reads as data';
}

{
    my $s = set <a b>;
    is $s.pairs.sort.raku, '(:a, :b).Seq', 'a Set .pairs is unchanged';
    my %bh is BagHash = a => 2, b => 3;
    is %bh.pairs.sort.raku, '(:a(2), :b(3)).Seq', 'a mutable QuantHash keeps its weight arm';
}

{
    my @sh[2;2] = (1, 2), (3, 4);
    is @sh.pairs.raku, '((0, 0) => 1, (0, 1) => 2, (1, 0) => 3, (1, 1) => 4).Seq',
        'a shaped array keeps the snapshot producer';
}

# --- the consumer side: a pair value is read as DATA -------------------------

{
    my %src = a => 1, b => 2, c => 3;
    my %copy = %src.pairs;
    %copy<a> = 99;
    is %src<a>, 1, 'Hash-from-.pairs copies, it does not alias';
    is-deeply %copy, {a => 99, b => 2, c => 3}, 'and the copy holds the values';
}

{
    my %src = a => 1, b => 2, c => 3;
    my %z is BagHash;
    %z = %src.pairs;
    is %z.sort.raku, '(:a(1), :b(2), :c(3)).Seq', 'BagHash-from-.pairs keeps the weights';
    is %src.pairs.Bag.sort.raku, '(:a(1), :b(2), :c(3)).Seq', '.pairs.Bag keeps the weights';
    is %src.pairs.Mix.sort.raku, '(:a(1), :b(2), :c(3)).Seq', '.pairs.Mix keeps the weights';
    is %src.pairs.MixHash.sort.raku, '(:a(1), :b(2), :c(3)).Seq',
        '.pairs.MixHash keeps the weights';
}

# The same read, reached without `.pairs` at all: a `key => $x` pair has
# carried a container since 2026-09-01, so this was already wrong on main.
{
    my $x = 3;
    my %z is BagHash;
    %z = ((a => $x),);
    is %z.raku, '("a"=>3).BagHash', 'a container-valued Pair weighs its VALUE, not its truthiness';
}

{
    my %src = a => 1, b => 2;
    is %src.pairs.map({ .key => .value }).sort.raku, '(:a(1), :b(2)).Seq',
        '.map({.key => .value}) rebuilds pairs from data';
    is %src.pairs.map({ .value }).sort.raku, '(1, 2).Seq', '.map({.value}) yields data';
    is %src.pairs>>.value.sort.raku, '(1, 2).Seq', 'hyper .value yields data';
}

{
    my %src = a => 1, b => 2;
    # Order-independent: only the KEY shape is at issue -- `.antipairs` must
    # put a de-itemized snapshot there, never the element's cell.
    is %src.pairs.antipairs.map({ .key.raku }).sort.raku, '(":a(1)", ":b(2)").Seq',
        '.antipairs over .pairs de-itemizes the key';
    is %src.pairs.invert.sort.raku, '(1 => "a", 2 => "b").Seq', '.invert over .pairs';
    is %src.pairs.Hash.raku, '{:a(1), :b(2)}', '.pairs.Hash';
    is %src.pairs.sort.raku, '(:a(1), :b(2)).Seq', '.pairs.sort';
}

# A `trans` matcher type-tests the pair's value to decide closure vs literal.
{
    my %m = a => { "X" }, b => "Y";
    is "ab".trans(%m.pairs), "XY", 'trans still sees a closure replacement through .pairs';
}

# --- storing a pair value is a COPY, not an alias ---------------------------
# `.value` returns the container (row 6 needs that for `.VAR`), so a *store*
# has to read through it. Only a bind aliases. These four store sites did not,
# and were already wrong on main for a plain `key => $x` pair.
{
    my %h1 = a => 1;
    my @e1; @e1.push(%h1.pairs[0].value); %h1<a> = 9;
    is-deeply @e1, [1], 'push stores a copy of a pair value';
}
{
    my %h2 = a => 1;
    my @e2; @e2.append(%h2.pairs[0].value); %h2<a> = 9;
    is-deeply @e2, [1], 'append stores a copy of a pair value';
}
{
    my %h3 = a => 1;
    my %o3; %o3<k> = %h3.pairs[0].value; %h3<a> = 9;
    is-deeply %o3, {k => 1}, 'a hash element assign stores a copy of a pair value';
}
{
    my %h4 = a => 1;
    my @e4 = [0]; @e4[0] = %h4.pairs[0].value; %h4<a> = 9;
    is-deeply @e4, [1], 'an array element assign stores a copy of a pair value';
}

# The promoted cell carries the container's element constraint (ADR-0036 row 12).
{
    my Int @t = 1, 2;
    dies-ok { @t.pairs[0].value = "s" }, 'a typed array rejects a bad write through .pairs';
    is @t.raku, 'Array[Int].new(1, 2)', 'and the array is unchanged';
}
