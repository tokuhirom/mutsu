use v6.e.PREVIEW;
use Test;

plan 41;

# ---------------------------------------------------------------------------
# Blob/Buf.contents -- the buffer's elements as a List (Type/Buf, and the
# `use experimental :pack` doc example that first surfaced the gap).
# ---------------------------------------------------------------------------
{
    my $b = Buf.new(1, 2, 3);
    is-deeply $b.contents, (1, 2, 3), 'Buf.contents yields the byte values';
    isa-ok $b.contents, List, 'Buf.contents returns a List';
    is-deeply $b.contents, $b.list, 'Buf.contents matches Buf.list';
    is-deeply Blob.new(5, 6).contents, (5, 6), 'Blob.contents yields the byte values';
    is-deeply Buf.new().contents, (), 'an empty Buf has empty contents';
    is-deeply buf16.new(300, 400).contents, (300, 400),
      'buf16.contents decodes at the buffer own element width';
    is-deeply utf8.new(65, 66).contents, (65, 66), 'utf8.contents yields the byte values';
}

# ---------------------------------------------------------------------------
# subbuf-rw as an lvalue -- in BOTH the method and the function-call form.
# The buffer is mutated in place, so a second reference to the SAME buffer
# object sees the write (a value-only check would pass on a rebuilt copy).
# ---------------------------------------------------------------------------
{
    my Buf $b .= new(1, 2, 3);
    my $same = $b;          # another reference to the same Buf object
    subbuf-rw($b, 2, 1) = Buf.new(42);
    is $b.raku, 'Buf.new(1,2,42)', 'subbuf-rw function form assigns';
    is $same.raku, 'Buf.new(1,2,42)',
      'subbuf-rw function form mutates the ORIGINAL buffer, not a copy';
}

{
    my Buf $b .= new(1, 2, 3);
    my $same = $b;
    $b.subbuf-rw(2, 1) = Buf.new(42);
    is $b.raku, 'Buf.new(1,2,42)', 'subbuf-rw method form assigns';
    is $same.raku, 'Buf.new(1,2,42)',
      'subbuf-rw method form mutates the ORIGINAL buffer, not a copy';
}

{
    my Buf $b .= new(1, 2, 3);
    subbuf-rw($b, 1, 1) = Buf.new(7, 8, 9);
    is $b.raku, 'Buf.new(1,7,8,9,3)', 'subbuf-rw grows the buffer';
}

{
    my Buf $b .= new(1, 2, 3, 4);
    subbuf-rw($b, 1, 2) = Buf.new(99);
    is $b.raku, 'Buf.new(1,99,4)', 'subbuf-rw shrinks the buffer';
}

{
    my Buf $b .= new(1, 2, 3);
    subbuf-rw($b, 1) = Buf.new(9);
    is $b.raku, 'Buf.new(1,9)', 'subbuf-rw with no length replaces the whole tail';
}

{
    my Buf $b .= new(1, 2, 3);
    subbuf-rw($b, 3, 1) = Buf.new(4);
    is $b.raku, 'Buf.new(1,2,3,4)', 'subbuf-rw at the end appends';
}

# ---------------------------------------------------------------------------
# .splice's one-arg rule: a lone Blob/Buf does Positional, so it contributes
# its ELEMENTS, exactly like a lone Array/List/Range does.
# ---------------------------------------------------------------------------
{
    my @a = 1, 2, 3;
    @a.splice(1, 1, Buf.new(1, 2));
    is-deeply @a, [1, 1, 2, 3], 'a lone Buf replacement arg flattens to its elements';
}

{
    my @a = 1, 2, 3;
    @a.splice(1, 1, Blob.new(9, 10));
    is-deeply @a, [1, 9, 10, 3], 'a lone Blob replacement arg flattens to its elements';
}

{
    my @a = 1, 2, 3;
    @a.splice(1, 1, utf8.new(65, 66));
    is-deeply @a, [1, 65, 66, 3], 'a lone utf8 replacement arg flattens to its elements';
}

{
    my @a = 1, 2, 3;
    @a.splice(1, 1, "xy");
    is-deeply @a, [1, "xy", 3], 'a lone Str replacement arg stays one element';
}

# ---------------------------------------------------------------------------
# .splice type-checks the values it actually stores -- after the one-arg rule
# and after Nil decays to Any (ADR-0049), not before.
# ---------------------------------------------------------------------------
{
    my Int @a = 1, 2, 3;
    throws-like { @a.splice(1, 0, "x") }, X::TypeCheck::Splice,
      message => 'Type check failed in splice; expected Int but got Str (Str)',
      'a wrong-typed splice insert throws X::TypeCheck::Splice';
    is-deeply @a, Array[Int].new(1, 2, 3), 'the rejected splice left the array alone';
}

{
    my Int @a = 1, 2, 3;
    throws-like { @a.splice(1, 0, Nil) }, X::TypeCheck::Splice,
      message => 'Type check failed in splice; expected Int but got Any (Any)',
      'a Nil splice insert into a typed array throws (it decays to Any)';
    is-deeply @a, Array[Int].new(1, 2, 3), 'the rejected Nil splice left the array alone';
}

{
    my @a = 1, 2, 3;
    @a.splice(1, 0, Nil);
    is @a.elems, 4, 'a Nil splice insert into an UNtyped array is allowed';
    ok !@a[1].defined, 'and stores an undefined value';
}

{
    my Int @a = 1, 2, 3;
    lives-ok { @a.splice(1, 1) }, 'a splice with no replacement values type-checks nothing';
    is-deeply @a, Array[Int].new(1, 3), 'and still removes the requested element';
}

{
    my Int @a = 1, 2, 3;
    my @b = 4, 5;
    @a.splice(1, 0, @b);
    is-deeply @a, Array[Int].new(1, 4, 5, 2, 3),
      'a lone Array of good elements splices in and type-checks per element';
}

{
    my Int @a = 1, 2, 3;
    my @b = 4, 5;
    my @c = 6, 7;
    throws-like { @a.splice(1, 0, @b, @c) }, X::TypeCheck::Splice,
      message => 'Type check failed in splice; expected Int but got Array (Array)',
      'several Array args do NOT flatten, so each is checked as an Array';
}

# ---------------------------------------------------------------------------
# .snip takes every positional as a matcher, advancing round-robin.
# ---------------------------------------------------------------------------
isa-ok (5, 13, 29).snip(* < 10), Seq, 'snip returns a Seq';
is (5, 13, 29).snip(* < 10, * < 20).raku, '((5,), (13,), (29,)).Seq',
  'snip honours a second positional matcher';
is (1, 2, 3, 4).snip(* < 3).raku, '((1, 2), (3, 4)).Seq',
  'snip with one matcher still snips once';
is (1 .. 12).snip(* < 4, * < 8, * < 11).raku,
  '((1, 2, 3), (4, 5, 6, 7), (8, 9, 10), (11, 12)).Seq',
  'snip honours three positional matchers';
is (2, 5, "a", "b", 7, 8).snip(Int, Str).raku,
  '((2, 5), ("a", "b"), (7, 8)).Seq',
  'snip accepts type-object matchers positionally';
is (5, 13, 29).snip((* < 10, * < 20)).raku, '((5,), (13,), (29,)).Seq',
  'a single list of matchers behaves the same as separate positionals';

# ---------------------------------------------------------------------------
# An object hash gists its ORIGINAL typed keys even when a value carries a
# custom .gist (which routes the render through the dispatching gist path).
# ---------------------------------------------------------------------------
{
    class Foo {}
    my %h{Any};
    %h{True} = Foo.new;
    is %h.gist, '{True => Foo.new}', 'an object hash gists a Bool key as True, not its WHICH';

    my %g{Any};
    %g{7} = Foo.new;
    is %g.gist, '{7 => Foo.new}', 'an object hash gists an Int key as 7, not its WHICH';
}

{
    my %h;
    my @a = (Foo.new,);
    @a.categorize({ True }, into => %h);
    is %h.gist, '{True => [Foo.new]}',
      'categorize into a hash renders its Bool bucket key as True';
}

# ---------------------------------------------------------------------------
# An array/hash mapper reports a miss as the Any type object -- the same thing
# the equivalent subscript read yields -- not as Nil.
# ---------------------------------------------------------------------------
{
    my @mapper = <zero one two three four five>;
    is MixHash.new.classify-list(@mapper, 1, 2, 3, 4, 4, 6).gist,
      'MixHash((Any) four(2) one three two)',
      'classify-list with an array mapper keys an out-of-range index by Any';

    my @m = <a b c>;
    is (0, 1, 5).classify(@m).gist, '{(Any) => [5], a => [0], b => [1]}',
      'classify with an array mapper keys an out-of-range index by Any';

    my %hm = a => 1;
    is ("a", "z").classify(%hm).gist, '{(Any) => [z], 1 => [a]}',
      'classify with a hash mapper keys a missing key by Any';
}
