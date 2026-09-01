use v6;
use Test;

# ADR-0040: Array and Hash elements are itemized at the *store*, not
# compensated at the read (docs/adr/0040-array-hash-elements-are-itemized-
# at-the-store.md).
#
# This file is the acceptance oracle from ADR-0040 SS1.3 (the 25-row
# divergence matrix, measured on `main` 52631889f 2026-08-20) plus SS1.6's
# agreeing rows, the SS2 arity invariants, and the SS2 negative (non-)
# itemization list. Slice 1 fixed the *mutation* sites (element assign,
# autovivification, push/unshift/append/prepend/splice for a real
# Array/Hash), turning rows 19-22 green. Slice 2 fixes the *construction*
# sites (list-assign into `@a`/`%h`, real-container literal construction,
# `.Array`/`.Hash` coercion), turning rows 01-18 and 23 green -- every
# downstream element producer inherits the flag (SS1.6.3). Slice 3 states the
# same model on the *reflection* side (`.VAR`), turning row 24 green. Row 25
# (and the SS1.6 rows) are invariants that must NOT move -- they are what
# stops a later slice from "fixing" the divergence by over-itemizing.

# === SS1.3 divergent rows (25) ===

my @c = [<a b>],[<c d>];
my %h = a => [1,2];
sub takes(*@a) { @a.elems }

{
    is @c[0].raku, '$["a", "b"]', 'row 01: my @c = [<a b>],[<c d>]; @c[0].raku';
}
{
    is %h<a>.raku, '$[1, 2]', 'row 02: my %h = a => [1,2]; %h<a>.raku';
}
{
    is @c[0,1].raku, '($["a", "b"], $["c", "d"])', 'row 03: @c[0,1].raku';
}
{
    is @c.head.raku, '$["a", "b"]', 'row 04: @c.head.raku';
}
{
    is @c.tail.raku, '$["c", "d"]', 'row 05: @c.tail.raku';
}
{
    is @c.first(*.so).raku, '$["a", "b"]', 'row 06: @c.first(*.so).raku';
}
{
    is @c.sort.raku, '($["a", "b"], $["c", "d"]).Seq', 'row 07: @c.sort.raku';
}
{
    is @c.reverse.raku, '($["c", "d"], $["a", "b"]).Seq', 'row 08: @c.reverse.raku';
}
{
    is @c.map({$_}).raku, '($["a", "b"], $["c", "d"]).Seq', 'row 09: @c.map({$_}).raku';
}
{
    is @c.pairs[0].value.raku, '$["a", "b"]', 'row 10: @c.pairs[0].value.raku';
}
{
    is @c.Slip.raku, 'slip($["a", "b"], $["c", "d"])', 'row 11: @c.Slip.raku';
}
{
    is takes(@c[0]), 1, 'row 12: takes(@c[0])';
}
{
    is takes(%h<a>), 1, 'row 13: takes(%h<a>)';
}
{
    is takes(@c.head), 1, 'row 14: takes(@c.head)';
}
{
    my $n;
    for @c { $n = takes($_) }
    is $n, 1, 'row 15: for @c { takes($_) }';
}
{
    is (my @z = @c[0]).elems, 1, 'row 16: (my @z = @c[0]).elems';
}
{
    is [@c[0]].elems, 1, 'row 17: [@c[0]].elems';
}
{
    is join('|', @c[0]), 'a b', "row 18: join('|', \@c[0])";
}

# --- rows 19-22: the mutation sites (Slice 1, THIS PR) ---

{
    my @a = 1, 2;
    @a[0] = (7, 8);
    is @a[0].raku, '$(7, 8)', 'row 19: @a[0] = (7,8); @a[0].raku';
}
{
    my @a = 1, 2;
    @a.push([7, 8]);
    is @a[2].raku, '$[7, 8]', 'row 20: @a.push([7,8]); @a[2].raku';
}
{
    my @a;
    @a[3] = [7, 8];
    is @a[3].raku, '$[7, 8]', 'row 21: my @a; @a[3] = [7,8]; @a[3].raku';
}
{
    my @a;
    @a.append([7, 8], [9, 0]);
    is @a[0].raku, '$[7, 8]', 'row 22: @a.append([7,8],[9,0]); @a[0].raku';
    is @a.elems, 2, 'row 22: @a.append([7,8],[9,0]); @a.elems is 2';
}

{
    my @a23 = (1..3), (4..6);
    is takes(@a23[0]), 1, 'row 23: my @a = (1..3),(4..6); takes(@a[0])';
}
{
    my @l := 1, (1, 2), [3, 4];
    is "{@l[1].VAR.^name} {@l[2].VAR.^name}", 'List Array',
        'row 24: @l[1].VAR.^name, @l[2].VAR.^name';
}

# row 25: the invariant. An Array's OWN .raku de-itemizes its elements --
# this must NOT move, or a later slice has over-itemized.
is @c.raku, '[["a", "b"], ["c", "d"]]', 'row 25: @c.raku stays bare (invariant)';

# === SS1.6 agreeing rows (must stay agreeing) ===

{
    my $n1;
    for ((1, 2), (3, 4)) { $n1 = takes($_) }
    is $n1, 2, 'SS1.6: List literal source, implicit topic, stays 2';
}
{
    my $n2;
    for (([1, 2], [3, 4])) { $n2 = takes($_) }
    is $n2, 2, 'SS1.6: List of Array literals, implicit topic, stays 2';
}
{
    my $n3;
    for @c -> $v { $n3 = takes($v) }
    is $n3, 1, 'SS1.6: real Array source, pointy param, stays 1 (bind-side)';
}

# === Slice 1 extras: the other mutation sites this PR fixes ===

{
    my %h2;
    %h2<a> = [1, 2];
    is %h2<a>.raku, '$[1, 2]', 'slice 1: %h<a> = [1,2]; %h<a>.raku';
}
{
    my @u = 1, 2;
    @u.unshift([9, 9]);
    is @u[0].raku, '$[9, 9]', 'slice 1: @u.unshift([9,9]); @u[0].raku';
}
{
    my @p = 1, 2;
    @p.prepend([8, 8]);
    is @p[0].raku, '8', 'slice 1: @p.prepend([8,8]) flattens (one-arg rule)';
    is @p.elems, 4, 'slice 1: @p.prepend([8,8]) flattens to 4 elements';
}
{
    my @b2 = 1, 2;
    @b2.push(1, [2, 3]);
    is @b2[3].raku, '$[2, 3]', 'slice 1: multi-arg push itemizes only the aggregate';
}

# === arity invariants (must NOT change) ===

{
    my @p1;
    @p1.push(1, 2);
    is @p1.elems, 2, 'arity: push(1,2) is 2 elements';
}
{
    my @a2;
    @a2.append((5, 4));
    is @a2.elems, 2, 'arity: append((5,4)) is 2 elements';
}

# === SS2 negatives: must NOT be wrapped ===

{
    my @a3;
    @a3[0] = (a => 1);
    is @a3[0].raku, ':a(1)', 'negative: a Pair element stays unwrapped';
}
{
    my @a4;
    @a4[0] = Set.new(1, 2);
    is @a4[0].raku, 'Set.new(1,2)', 'negative: a Set element stays unwrapped';
}
{
    my @a5;
    @a5[0] = 5;
    is @a5[0].raku, '5', 'negative: an Int element stays unwrapped';
}

# === native-array safety (must not corrupt native storage) ===

{
    my int @n;
    @n.push(5);
    is @n[0], 5, 'native: int @n.push(5) stores a plain native Int';
}

# === further Slice 1 mutation sites: nested autovivification, Hash.push,
# and reference-shared push all itemize too ===

{
    # Nested autovivification: `@a[5][0] = 1` autovivifies `@a[5]` as a
    # fresh Array, and that freshly-stored intermediate element itemizes
    # like any other Array/Hash element store.
    my @a6;
    @a6[5][0] = 1;
    is @a6[5].raku, '$[1]', 'slice 1: @a[5][0] = 1; @a[5].raku';
}
{
    # Nested hash autovivification: `%h<a><b> = 1` autovivifies `%h<a>` as
    # a fresh Hash, itemized the same way.
    my %h4;
    %h4<a><b> = 1;
    is %h4<a>.raku, '${:b(1)}', 'slice 1: %h<a><b> = 1; %h<a>.raku';
}
{
    # Hash.push (the slow-path method dispatch, not the `%h<k> = v` opcode)
    # itemizes the pushed pair's value the same way.
    my %h3;
    %h3.push('a' => [1, 2]);
    is %h3<a>.raku, '$[1, 2]', 'slice 1: %h.push(pair) itemizes the pushed value';
}
{
    # Reference-shared push (`@a.push(@b)`, a NAMED array): raku's
    # non-flattening `**@values` slurpy stores the container itself, kept
    # live-aliased, AND itemized on the pushed element -- while `@b` read
    # directly stays bare (the two readers of the shared cell disagree on
    # itemization, matching raku exactly).
    my @b3 = (1, 2);
    my @a7;
    @a7.push(@b3);
    is @a7[0].raku, '$[1, 2]', 'slice 1: @a.push(@b) (reference-shared); @a[0].raku';
    is @b3.raku, '[1, 2]', 'slice 1: @b itself stays bare after the reference push';
    @b3.push(3);
    is @a7[0].raku, '$[1, 2, 3]', 'slice 1: mutating @b propagates through the shared cell';
}

# === CI regression (roast/integration/advent2010-day11.t crashed): an
# itemized element must decompose into its OWN elements for `.pick`/`.roll`/
# `.head`/`.tail`, not be treated as a single opaque item. Itemization
# governs how a value flattens as an ELEMENT of some OTHER container; it
# must not change how a value decomposes when it is itself the receiver of
# one of these methods. ===

{
    # Nested-autovivified Hash element (`%h<a><b>++`): `.roll` on it must
    # roll one of ITS OWN pairs, not return the whole (itemized) hash.
    my %outer;
    %outer{"x"}{"y"}++;
    my $rolled = %outer{"x"}.roll;
    is $rolled.WHAT, Pair, 'roll on itemized Hash element returns a Pair';
    is $rolled.raku, ':y(1)', 'roll on itemized Hash element rolls its own pair';
}

{
    # Same for `.pick` on an itemized Hash element.
    my %outer2;
    %outer2{"x"}{"y"}++;
    my $picked = %outer2{"x"}.pick;
    is $picked.WHAT, Pair, 'pick on itemized Hash element returns a Pair';
}

{
    # Itemized Array element (`@a.push([1,2,3])`): `.roll`/`.pick` must
    # return one of its own elements, not the whole itemized array.
    my @a;
    @a.push([1, 2, 3]);
    my $rolled = @a[0].roll;
    is $rolled.WHAT, Int, 'roll on itemized Array element returns an element';
    ok (1 <= $rolled <= 3), 'roll on itemized Array element is in range';
    my $picked = @a[0].pick;
    is $picked.WHAT, Int, 'pick on itemized Array element returns an element';
}

{
    # `.head`/`.tail` on an itemized Array element likewise decompose into
    # elements, not the whole itemized array.
    my @a2;
    @a2.push([1, 2, 3]);
    is @a2[0].head, 1, 'head on itemized Array element returns its first element';
    is @a2[0].tail, 3, 'tail on itemized Array element returns its last element';
}

# === Slice 2: the construction sites ===

{
    # A plain (non-declaring) list-assign itemizes exactly like the `my`
    # declaration form.
    my @a;
    @a = [1, 2], [3, 4];
    is @a[0].raku, '$[1, 2]', 'slice 2: @a = [1,2],[3,4] (non-decl assign)';
    my %h;
    %h = a => [1, 2];
    is %h<a>.raku, '$[1, 2]', 'slice 2: %h = a => [1,2] (non-decl assign)';
}
{
    # An array/hash LITERAL's elements itemize too (the `[...]` / `%(...)`
    # construction ops), while a `(...)` List literal's do NOT (SS1.6).
    is [[1, 2], [3, 4]][0].raku, '$[1, 2]', 'slice 2: [[1,2],[3,4]][0].raku';
    is ((1, 2), (3, 4))[0].raku, '(1, 2)',
        'slice 2: a List literal element stays bare (invariant)';
    is %(a => [1, 2])<a>.raku, '$[1, 2]', 'slice 2: %(a => [1,2])<a>.raku';
}
{
    # `.Array` builds a real Array (elements are containers); `.list` and
    # `.List` do not build one -- and `.List` on a real Array hands out each
    # element's VALUE, so it decontainerizes (measured against raku:
    # `@c.List[0].VAR.^name` is `Array`, `@c.list[0].VAR.^name` is `Scalar`).
    is ((1, 2), (3, 4)).Array[0].raku, '$(1, 2)', 'slice 2: .Array itemizes';
    my @c2 = [1, 2], [3, 4];
    is @c2.List[0].raku, '[1, 2]', 'slice 2: .List on a real Array de-itemizes';
    is @c2.list[0].raku, '$[1, 2]', 'slice 2: .list keeps the containers';
    is ($[1, 2],).List[0].raku, '$[1, 2]',
        'slice 2: .List on a List is identity (invariant)';
}
{
    # A copy of an array whose elements are already itemized is a no-op --
    # this is the path SS5.2 protects (`my @a = @b` keeps sharing the Gc).
    my @a = [1, 2], [3, 4];
    my @b = @a;
    is @b[0].raku, '$[1, 2]', 'slice 2: my @b = @a keeps the itemization';
    is @b.raku, '[[1, 2], [3, 4]]', 'slice 2: ...and @b.raku stays bare';
}
{
    # Every stored aggregate KIND the ADR SS2 names, at a construction site.
    my @s = (1, 2).Seq, (3, 4).Seq;
    is @s[0].raku, '$((1, 2).Seq)', 'slice 2: a stored Seq itemizes';
    my @r = (1 .. 3), (4 .. 6);
    is takes(@r[0]), 1, 'slice 2: a stored Range is one item';
    my @h = {a => 1}, {b => 2};
    is @h[0].raku, '${:a(1)}', 'slice 2: a stored Hash itemizes';
}
{
    # A hash built from a flat list (the `build_hash_from_items` path used by
    # `.Hash` and by an odd/even list assign) itemizes its values too.
    my %h = ('a', [1, 2], 'b', [3, 4]);
    is %h<a>.raku, '$[1, 2]', 'slice 2: flat-list hash construction itemizes';
    my %g = (a => [1, 2]).List.Hash;
    is %g<a>.raku, '$[1, 2]', 'slice 2: .Hash coercion itemizes';
}
{
    # A gather/take reified into an array goes through the LazyList arm that
    # bypasses `coerce_to_array`; it must itemize too.
    my @g = gather { take [1, 2]; take [3, 4] };
    is @g[0].raku, '$[1, 2]', 'slice 2: a reified gather itemizes its elements';
}
{
    # A `:=` bind must NOT itemize -- a bound List's elements are not
    # containers (SS1.6 / row 24's model, seen from the value side).
    my @l := 1, (1, 2), [3, 4];
    is @l[1].raku, '(1, 2)', 'slice 2: a bound List element stays bare (invariant)';
    is @l[2].raku, '[3, 4]', 'slice 2: ...including an Array element of a bound List';
}
{
    # Arity is untouched by the construction hook (SS2 part 3).
    my @f = flat [1, 2], [3, 4];
    is @f.elems, 4, 'slice 2: flat still flattens to 4 elements';
    my @n = (1, 2), (3, 4);
    is @n.elems, 2, 'slice 2: a two-element list-assign is still 2 elements';
    my @one = [1, 2];
    is @one.elems, 2, 'slice 2: the one-arg rule still flattens a lone Array';
}
{
    # A native array's storage must not be disturbed by the scan.
    my int @n = 1, 2, 3;
    is @n[0].raku, '1', 'slice 2: native int array elements stay plain';
    is @n.elems, 3, 'slice 2: native int array keeps its elements';
}

# === Slice 2 counter-currents: sites that ask a question about the VALUE
# while holding something itemized because it is an ELEMENT. Same family as
# Slice 1's `value_to_list_for_receiver` discovery; every one of these was
# found by the local test suite, not by reading. ===

{
    # `.antipairs` is `self.pairs.map: *.antipair`, and `Pair.antipair` READS
    # `$!value` to build the new key -- an attribute read decontainerizes. So
    # the same element is itemized as a pair's VALUE and bare as its KEY.
    my @c;
    @c[0] = [1, 2];
    is @c.pairs.raku, '(0 => $[1, 2],).Seq', 'counter-current: .pairs keeps the value itemized';
    is @c.antipairs.raku, '([1, 2] => 0,).Seq', 'counter-current: .antipairs de-itemizes the key';
}
{
    # `.invert` expands an iterable value into one pair per member -- the
    # element's own itemization must not stop that.
    # (sorted: hash iteration order is not specified)
    is {a => (1, 2), b => 3 .. 4}.invert.sort.raku,
        '(1 => "a", 2 => "a", 3 => "b", 4 => "b").Seq',
        'counter-current: .invert expands an itemized hash value';
}
{
    # An array's own `.raku` de-itemizes its elements (row 25) -- including
    # through a `:=`-bound element's ContainerRef cell, so a bound element and
    # its un-bound sibling agree.
    my @a = {p => 1}, {q => 2};
    my $w := @a[0];
    $w<p> = 100;
    is @a.raku, '[{:p(100)}, {:q(2)}]',
        'counter-current: row 25 sees through a bound element cell';
}
{
    # deepmap's leaf-vs-descend test is about what the value IS; a stored
    # Range is itemized but still descends. (The RESULT is itemized because
    # its parent is a Hash, which is decided separately.)
    is %(a => 1, b => (2 .. 3)).deepmap(* + 1).raku, '{:a(2), :b($(3, 4))}',
        'counter-current: deepmap descends into an itemized Range element';
}
{
    # Binding an element to an `@` sub-signature parameter reads the element's
    # VALUE, so it decontainerizes -- the same rule as `my @a := @c[0]`.
    my &g = -> [@a, $b] { "{@a.^name}:{@a.elems}" };
    is g([(1, 2).Seq, 9]), 'List:2',
        'counter-current: a Seq element binds to an @ sub-parameter';
    is g([[7, 8], 9]), 'Array:2',
        'counter-current: an Array element binds to an @ sub-parameter';
}
{
    # An itemized array is still a real array AS A RECEIVER: `*-2` resolves
    # against its own element count.
    my @w = [1, 2, 3, 4],;
    my @r = @w[0].splice(*-2, 1);
    is @r.raku, '[3]', 'counter-current: splice(*-2) on an itemized element receiver';
    is @w.raku, '[[1, 2, 4],]', 'counter-current: ...and it mutates the right slot';
}
{
    # `my (@a, @b) := (...)` BINDS each target to the staged element (which the
    # construction hook itemized), so it decontainerizes; the `=` form assigns
    # and keeps raku's greedy-slurp semantics.
    my @x = 1, 2;
    my @y = 5;
    my (@a, @b) := (@x, @y);
    is @a.raku, '[1, 2]', 'counter-current: := destructure binds the element';
    is @b.raku, '[5]', 'counter-current: ...including the trailing target';
    my (@c3, @d) = (@x, @y);
    is @c3.raku, '[[1, 2], [5]]', 'counter-current: = destructure still slurps greedily';
    is @d.raku, '[]', 'counter-current: ...leaving later targets empty';
}

{
    # A reduction's operands are the element VALUES of its source list, so an
    # element itemized *because it is an element* is handed to the operator
    # decontainerized -- while the explicit infix form receives the elements
    # themselves and does NOT zip them. Both measured against raku.
    my @m = [1, 2], [3, 4];
    is ([Z] @m).raku, '((1, 3), (2, 4)).Seq',
        'counter-current: [Z] reads the element values';
    is (@m[0] Z @m[1]).raku, '(($[1, 2], $[3, 4]),).Seq',
        'counter-current: ...while an explicit Z receives the elements';
    is ([+] @m[0]).raku, '2',
        'counter-current: a lone itemized operand keeps its itemization';
}
{
    # `.Array` builds a NEW real Array, which is not an element of anything --
    # so an itemized receiver's own itemization is dropped, exactly as `.list`
    # already drops it, while the new array's own elements itemize.
    my @a = [1, 2], [3, 4];
    is @a[0].Array.raku, '[1, 2]', 'counter-current: .Array drops the receiver itemization';
    is ((1, 2), (3, 4)).Array[0].raku, '$(1, 2)',
        'counter-current: ...and still itemizes the new array elements';
}
{
    # A decoded JSON object/array is a real Hash/Array, so its aggregate
    # values/elements are containers.
    use JSON::Fast;
    my $d = from-json('{"a":[1,2],"b":{"c":3}}');
    is $d<a>.raku, '$[1, 2]', 'slice 2: from-json object values itemize';
    is $d<b>.raku, '${:c(3)}', 'slice 2: ...including a nested object';
    my $l = from-json('[[1,2],[3,4]]');
    is $l[0].raku, '$[1, 2]', 'slice 2: from-json array elements itemize';
    is to-json($l, :!pretty), '[[1,2],[3,4]]', 'slice 2: ...and to-json round-trips unchanged';
}

{
    # Set-operator membership: the CONTAINER is the receiver of the test, so
    # its own element-itemization is not part of the question. (The needle is
    # deliberately NOT decontainerized -- a Set's members keep their
    # itemization, so `.WHICH` membership must see what was stored.)
    my @e = 2, 1 .. 2;
    ok (@e[0] (elem) @e[1]), 'counter-current: (elem) decomposes an itemized Range container';
    ok (@e[1] (cont) @e[0]), 'counter-current: ...and (cont) likewise';
}
{
    # `.Map` decontainerizes its values (a Map's values are not containers),
    # including the kind/flag form of itemization, not just a Scalar wrapper.
    my %h = a => [1, 2];
    my %m := %h.Map;
    is %m<a>.raku, '[1, 2]', 'counter-current: .Map deconts an itemized hash value';
    my class Foo { has @.a }
    my %args = a => [1, 2, 3];
    is Foo.new(|%args.Map).a.raku, '[1, 2, 3]',
        'counter-current: ...so |%args.Map binds an @ attribute element-wise';
}
{
    # `is-deeply` normalizes a Seq to a List before comparing; it has to see
    # through an element's itemization to find the Seq.
    is-deeply (1, 2).Seq, $((1, 2).Seq),
        'counter-current: is-deeply sees through a Scalar-wrapped Seq';
}
{
    # `.toggle` decomposes its own RECEIVER; a Hash element carries its
    # itemization as a flag, which must not make an empty hash look like one
    # item. And `<>` clears that flag, like it already cleared an ArrayKind's.
    my @t = %(), Map.new;
    is @t[0].toggle.raku, '().Seq', 'counter-current: .toggle decomposes an itemized Hash receiver';
    is @t[1].toggle.raku, '().Seq', 'counter-current: ...and an itemized Map receiver';
    my %h;
    is ($%h)<>.raku, '{}', 'counter-current: <> clears the hash itemization flag';
}

{
    # Smartmatch is `$matcher.ACCEPTS($topic)`, and both sides decontainerize
    # on the way in -- so an itemized element is transparent on both.
    my @t = [<42+0i>, 10 .. 50],;
    ok (@t[0][0] ~~ @t[0][1]), 'counter-current: smartmatch sees through an itemized matcher';
}
{
    # The `...` operator DECOMPOSES its seed operand into deduction seeds.
    is ($(1, 2) ... 10).head(5).List.raku, '(1, 2, 3, 4, 5)',
        'counter-current: ... decomposes an itemized seed';
    my @s = [(1/4, 1/2, 1), (8, 9)];
    @s.push(*);
    is infix:<...>(|@s).head(6).List.raku, '(0.25, 0.5, 1.0, 2.0, 4.0, 8)',
        'counter-current: ...and a chained one staged in an Array';
}
{
    # The n-arg receiver family: `.pairup`, `.head(n)`, `.pick(*)`,
    # `.combinations` all decompose their own receiver.
    is [[2, 3], [4, [5, 6]]]».pairup.gist, '((2 => 3) (4 => [5 6]))',
        'counter-current: .pairup decomposes an itemized receiver';
    is [[2, 3], [4, [5, 6]]]».pick(*)».sort.gist, '((2 3) (4 [5 6]))',
        'counter-current: .pick(*) is nodal over itemized elements';
    is [[2, 3], [4, [5, 6]]]».head(1).gist, '((2) (4))',
        'counter-current: .head(n) likewise';
    my @n := %(:42foo, :70bar),;
    is-deeply combinations(@n[0], 1).sort, @n[0].combinations(1).sort,
        'counter-current: &combinations(Iterable, k) matches the method form';
}
{
    # A grouped `.trans` operand means "expand each of these in turn", so an
    # itemized Range element still expands.
    is "Whfg".trans('a' .. 'z' => ['n' .. 'z', 'a' .. 'm']), 'Wust',
        'counter-current: .trans expands itemized Range group members';
}
{
    # The list-destructuring staging temp is NOT a user Array: every target
    # reads a VALUE out of it. A `%` target must still flatten the staged hash,
    # a `:=` `@` target binds (and so aliases), and a `*@rest` slurpy gets an
    # Array.
    sub named_and_slurp(:$grass, *%rest) { return ($grass, %rest) }
    my ($grass, %rest) = named_and_slurp(sky => 'blue', grass => 'green', fire => 'red');
    is $grass, 'green', 'staging temp: explicit named arg survives';
    is +%rest, 2, 'staging temp: a % target still flattens the staged hash';
    my ($x, @y, *@r) := (42, [13, 17], 5, 6, 7);
    is @y.raku, '[13, 17]', 'staging temp: a := @ target binds the element';
    is @r.raku, '[5, 6, 7]', 'staging temp: a slurpy *@rest is still an Array';
    my @xs = 1, 2;
    my (@a2,) := (@xs,);
    @a2.push(3);
    is @xs.raku, '[1, 2, 3]', 'staging temp: a := @ target aliases its source';
    my ($b) = ();
    is $b.^name, 'Any', 'staging temp: a missing untyped target is Any, not Nil';
}

# === Slice 3: `.VAR` reflection -- the discriminator, seen from the
# reflection side (ADR-0040 SS4 slice 3) ===
#
# A real, mutable Array/Hash stores each element in a Scalar container; a
# List/Seq stores the values themselves. Slices 1-2 put the *representation*
# half of that at the store (an itemized element renders `$[...]` and counts
# as one item), but a bare `Int` element cannot carry a flag, so `.VAR` has to
# read the answer off the SOURCE container's kind --
# `Value::elements_are_containers`. Every expectation below is dual-oracled
# against `raku`.
{
    my @real = 1, (1, 2), [3, 4];
    is @real[0].VAR.^name, 'Scalar', 'slice 3: real Array element is a Scalar (Int)';
    is @real[1].VAR.^name, 'Scalar', 'slice 3: real Array element is a Scalar (List)';
    is @real[2].VAR.^name, 'Scalar', 'slice 3: real Array element is a Scalar (Array)';

    my @bound := 1, (1, 2), [3, 4];
    is @bound[0].VAR.^name, 'Int', 'slice 3: bound List element is its own value (Int)';
    is @bound[1].VAR.^name, 'List', 'slice 3: bound List element is its own value (List)';
    is @bound[2].VAR.^name, 'Array', 'slice 3: bound List element is its own value (Array)';
    is @bound[2].VAR.raku, '[3, 4]', 'slice 3: .VAR on a List element is the value itself';
    is @bound[2].VAR.elems, 2, 'slice 3: .VAR on a List element keeps the value methods';

    # The source container, not the syntax that reads it (SS1.6.2).
    my $sl = (1, (1, 2), [3, 4]);
    is $sl[1].VAR.^name, 'List', 'slice 3: a List in a $ scalar still has bare elements';
    my \sless = (1, (1, 2), [3, 4]);
    is sless[2].VAR.^name, 'Array', 'slice 3: a sigilless List alias has bare elements';
    my @cached := (1, (1, 2), [3, 4]).Seq.cache;
    is @cached[2].VAR.^name, 'Array', 'slice 3: a cached Seq has bare elements';
    my $sq = (1, (2, 3), [4, 5]).Seq;
    is $sq[2].VAR.^name, 'Array', 'slice 3: a Seq in a $ scalar has bare elements';
    my @rb := 1 .. 3;
    is @rb[1].VAR.^name, 'Int', 'slice 3: a bound Range has bare elements';
    my $rs = 1 .. 3;
    is $rs[1].VAR.^name, 'Int', 'slice 3: a Range in a $ scalar has bare elements';
    my $lz = lazy gather { take $_ for 1, (2, 3) };
    is $lz[1].VAR.^name, 'List', 'slice 3: a lazy Seq in a $ scalar has bare elements';

    # Real-Array kinds all keep Scalar elements, including the ones on which
    # `ArrayKind::itemize()` is a no-op, and the lazy sources an `@`-assign
    # reifies (raku rejects binding a Seq to `@`, so an `@`-sigiled lazy list
    # can only be a real Array).
    my @lazy = ^Inf;
    is @lazy[1].VAR.^name, 'Scalar', 'slice 3: a lazy Array element is a Scalar';
    my @lza = lazy gather { take $_ for 1, (2, 3) };
    is @lza[1].VAR.^name, 'Scalar', 'slice 3: an @-assigned lazy gather element is a Scalar';
    my @sqa = (1, (2, 3)).Seq;
    is @sqa[1].VAR.^name, 'Scalar', 'slice 3: an @-assigned Seq element is a Scalar';
    my @rga = 1 .. 3;
    is @rga[1].VAR.^name, 'Scalar', 'slice 3: an @-assigned Range element is a Scalar';
    my @empty;
    is @empty[0].VAR.^name, 'Scalar', 'slice 3: an unfilled Array element is a Scalar';
    my %hh = a => [1, 2];
    is %hh<a>.VAR.^name, 'Scalar', 'slice 3: a Hash element is a Scalar';
    my %mm := Map.new((a => [1, 2]));
    is %mm<a>.VAR.^name, 'Array', 'slice 3: a Map value is NOT a container';

    # An unnamed subscript answers from the value side alone, and already
    # agreed before this slice -- it must keep agreeing.
    is (1, (2, 3))[1].VAR.^name, 'List', 'slice 3: List literal element (invariant)';
    is [1, (2, 3)][1].VAR.^name, 'Scalar', 'slice 3: Array literal element (invariant)';

    # Rakudo names an element container after the container it lives in.
    is @real[0].VAR.name, '@real', 'slice 3: an Array element .VAR.name is the array name';
    is %hh<a>.VAR.name, '%hh', 'slice 3: a Hash element .VAR.name is the hash name';
    is @real.VAR.name, '@real', 'slice 3: the array own .VAR.name is unchanged';
}

# === Slice 4: chained-subscript and deferred-vivification stores
# (ADR-0040 SS4 slice 4) ===
#
# Slice 1 hooked the single-level element assign and recorded that "deeper
# (3+-level) chained assignment was not separately audited". It was not
# covered -- and neither was the LEAF of a TWO-level chain, nor the
# intermediate a deferred vivification token walk-creates. All three stored
# bare, and the render-side compensator (`raku_hash_value`) hid it for
# `%h.raku` while every other reader disagreed: exactly SS1.5's "one value,
# three answers" shape, still alive one level down. Every expectation below is
# dual-oracled against `raku`.

# The value assigned through a chained subscript is an element store.
{
    my %h1; %h1<a><b> = [1, 2];
    is %h1<a><b>.raku, '$[1, 2]', 'slice 4: two-level hash chain leaf is itemized';
    is takes(%h1<a><b>), 1, 'slice 4: two-level hash chain leaf is one item';
    my @a1; @a1[0][1] = [1, 2];
    is @a1[0][1].raku, '$[1, 2]', 'slice 4: two-level array chain leaf is itemized';
    my %h2; %h2<a><b><c> = [1, 2];
    is %h2<a><b><c>.raku, '$[1, 2]', 'slice 4: three-level hash chain leaf is itemized';
    my @a2; @a2[0][1][2] = [1, 2];
    is @a2[0][1][2].raku, '$[1, 2]', 'slice 4: three-level array chain leaf is itemized';
    my %h3; %h3<a>[0] = (1, 2);
    is %h3<a>[0].raku, '$(1, 2)', 'slice 4: a List leaf through a mixed chain is itemized';
    my @a3; @a3[0]<k> = [1, 2];
    is @a3[0]<k>.raku, '$[1, 2]', 'slice 4: an array-then-hash chain leaf is itemized';
}

# The container mutsu autovivifies on the way down the chain is itself a
# stored element, so it itemizes too.
{
    my %h4; %h4<a><b><c> = 1;
    is %h4<a><b>.raku, '${:c(1)}', 'slice 4: a 3-level autovivified hash is itemized';
    my @a4; @a4[0][1][2] = 7;
    is @a4[0][1].raku, '$[Any, Any, 7]', 'slice 4: a 3-level autovivified array is itemized';
    is takes(@a4[0][1]), 1, 'slice 4: a 3-level autovivified array is one item';
    my %h5; %h5<a><b>[2] = 'z';
    is %h5<a><b>.raku, '$[Any, Any, "z"]', 'slice 4: hash-hash-array autovivification';
    my %h6; %h6<a>[0]<k> = 5;
    is %h6<a>.raku, '$[{:k(5)},]', 'slice 4: hash-array-hash autovivification';
    is %h6<a>[0].VAR.^name, 'Scalar', 'slice 4: a chained element reflects as a Scalar';
}

# A deferred vivification token (`my $r := %h<a><b>`) walk-creates the same
# intermediates at the first write, through a different mechanism.
{
    my %d1; my $r1 := %d1<a><b>; $r1 = 'x';
    is %d1<a>.raku, '${:b("x")}', 'slice 4: deferred 2-level walk-create is itemized';
    my %d2; my $r2 := %d2<a><b><c>; $r2 = 'x';
    is %d2<a><b>.raku, '${:c("x")}', 'slice 4: deferred 3-level walk-create is itemized';
    my %d3; my $r3 := %d3<a>[1]; $r3 = 'x';
    is %d3<a>.raku, '$[Any, "x"]', 'slice 4: deferred hash-array walk-create is itemized';
    my @d4; my $r4 := @d4[0][1]; $r4 = 'x';
    is @d4[0].raku, '$[Any, "x"]', 'slice 4: deferred array-array walk-create is itemized';
    my %d5; my $r5 := %d5<a><b>; $r5 = 'x';
    is %d5.raku, '{:a(${:b("x")})}', 'slice 4: the deferred whole-hash render is unchanged';
}

# A `:=` through a chain binds the source container: the element renders
# itemized while the source read directly stays bare, and writes still go
# through -- the same representation slice 1 chose for `@a.push(@b)`.
{
    my @s1 = 1, 2;
    my %b1; %b1<a><b> := @s1;
    is %b1<a><b>.raku, '$[1, 2]', 'slice 4: a two-level bound chain element is itemized';
    @s1.push(3);
    is %b1<a><b>.raku, '$[1, 2, 3]', 'slice 4: the two-level bind still writes through';
    is @s1.raku, '[1, 2, 3]', 'slice 4: the bind source read directly stays bare';
    # A 3+-level bind installs a shared `ContainerRef` cell instead of a value,
    # and wrapping THAT in a `Scalar` (slice 1's `@a.push(@b)` shape) breaks the
    # write path, which does not yet see through a `Scalar`-wrapped cell. Left
    # bare and recorded in ADR-0040 SS8; the write-through is what must not move.
    my @s2 = 4, 5;
    my %b2; %b2<a><b><c> := @s2;
    @s2.push(6);
    is %b2<a><b><c>.elems, 3, 'slice 4: a three-level bind still writes through';
}

# The invariants: a container's OWN .raku still de-itemizes its elements
# (row 25), the chain's arity is unchanged, and the assignment expression
# yields the (itemized) container.
{
    my %h7; %h7<a><b><c> = 1;
    is %h7.raku, '{:a(${:b(${:c(1)})})}', 'slice 4: %h.raku is unchanged (invariant)';
    my @a7; @a7[0][1][2] = 7;
    is @a7.raku, '[[Any, [Any, Any, 7]],]', 'slice 4: @a.raku is unchanged (invariant)';
    my %h8;
    my $r = (%h8<a><b> = [1, 2]);
    is $r.raku, '$[1, 2]', 'slice 4: the chained-assign expression value is itemized';
    is takes($r), 1, 'slice 4: the chained-assign expression value is one item';
}

# --- Slice 4b: the constructor IS the store -------------------------------
# A Hash a native Rust builtin builds is stored the same way an assignment
# stores one, so every reader agrees about its values; and the associative
# things mutsu represents with the same repr but whose values are NOT element
# containers (a Map, a Match's capture map, a slurpy `*%h`) stay bare.
{
    # `.classify` builds its Hash in Rust. BOUND, not assigned, so no Raku
    # assignment store can paper over the construction.
    my %c := (1, 2, 3, 4).classify({ $_ % 2 });
    is %c{0}.raku, '$[2, 4]', 'slice 4b: a natively built Hash itemizes its values';
    is %c{0}.VAR.^name, 'Scalar', 'slice 4b: ...so the element is a container';
    is takes(%c{0}), 1, 'slice 4b: ...and it is one item in list context';
    is %c.values.sort.raku, '($[1, 3], $[2, 4]).Seq',
        'slice 4b: .values agrees with the subscript read';
}
{
    # counter-current: a Map's values are not containers, so neither `Map.new`
    # nor `.Map` may itemize them -- and the read must not re-itemize either.
    my %m := Map.new((a => (1, 2)));
    is %m<a>.raku, '(1, 2)', 'slice 4b: Map.new does not itemize its values';
    is takes(%m<a>), 2, 'slice 4b: ...so a Map value spreads in list context';
    is %m.raku, 'Map.new((:a((1, 2))))',
        'slice 4b: ...and the Map renders its values bare too';
    my %src = a => (1, 2);
    is %src.Map<a>.raku, '(1, 2)', 'slice 4b: .Map deconts a List value too';
    # A non-Hash receiver folds to a Hash first (which itemizes, because that
    # is what a Hash store does) and must be deconted just the same --
    # roast/S32-hash/map.t's "Map does not introduce bogus Scalar containers".
    is ((a => (1, 2, 3)), (b => 4)).Map<a>.raku, '(1, 2, 3)',
        'slice 4b: .Map on a list of Pairs deconts too';
    my class MapBind { has Int @.a }
    lives-ok { MapBind.new(|((a => (1, 2, 3)), (b => 4)).Map) },
        'slice 4b: ...so |%.Map binds an Int @ attribute element-wise';
}
{
    # counter-current: a slurpy `*%h` is bound from the call's capture, so its
    # values are not containers (raku: `%h<a>.VAR.^name` is `List`). Read
    # inside the sub -- returning through `my $v =` would itemize on the way.
    # (`.VAR` still answers `Scalar` here rather than raku's `List` -- the
    # per-hash "are my values containers" bit does not exist yet; recorded in
    # todo/tickets/var-on-a-bare-valued-hash-answers-scalar.md.)
    sub slurped(*%h) { (%h<a>.raku, takes(%h<a>)).join('|') }
    is slurped(a => ('x', 'y')), '("x", "y")|2',
        'slice 4b: a slurpy %-param does not itemize its values';
    # ...while a plain `%`-param receiving a real Hash keeps that Hash's own
    # element itemization.
    sub plain(%h) { (%h<a>.raku, %h<a>.VAR.^name, takes(%h<a>)).join('|') }
    is plain({a => ('x', 'y')}), '$("x", "y")|Scalar|1',
        'slice 4b: a plain %-param still sees an itemized Hash value';
}
{
    # counter-current: a Match's capture map is not a Hash of containers --
    # a quantified capture reads back as the Array itself.
    'abab' ~~ / [$<x>=[a] b]+ /;
    is $/.hash<x>.VAR.^name, 'Array', 'slice 4b: a Match capture map stays bare';
    is takes($/.hash<x>), 2, 'slice 4b: ...so a quantified capture spreads';
}

done-testing;
