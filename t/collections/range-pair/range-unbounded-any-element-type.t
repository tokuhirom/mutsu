use Test;

# An upward-unbounded Range iterates lazily by `.succ` from its start,
# whatever the element type: Int (`1..*`, `^Inf`), Rat (`1.5..*`), Num
# (`1e0..Inf`) or Str (`"a"..*`). Every operation below used to reify a capped
# prefix (100k or 1M elements) for every start type but a plain Int, which was
# slow and, past the cap, wrong.

plan 40;

# -- element-producing operations, one row per start type ---------------------

for (1 .. *, (1, 2, 3), 'Int'),
    (^Inf, (0, 1, 2), 'Int ^Inf'),
    (1.5 .. *, (1.5, 2.5, 3.5), 'Rat'),
    ('a' .. *, <a b c>, 'Str') -> ($r, @want, $what) {
    is-deeply $r.head(3).List, @want.List, "$what: .head(n)";
    is-deeply $r[^3].List, @want.List, "$what: [^n]";
    is-deeply $r.list.head(3).List, @want.List, "$what: .list stays lazy";
}

is ('a' .. *).head, 'a', 'Str: argless .head';
is (1.5 .. *).head, 1.5, 'Rat: argless .head';
is-deeply (1.5 .. *).head(2).map(*.^name).List, <Rat Rat>, 'Rat elements stay Rat';
is ('a' ^.. *).head(2).List, <b c>, 'an excluded start steps past it';

# -- past the old caps ---------------------------------------------------------

is (1 .. *).first(* > 1_000_005), 1_000_006, '.first past the old 1M cap';
is (1 .. *).first(* > 1_000_005, :k), 1_000_005, '.first(:k) past the old 1M cap';
is (1.5 .. *)[1_000_001], 1_000_002.5, '[i] past the old 1M cap';
{
    my $seen;
    for ^Inf { if $_ > 1_000_002 { $seen = $_; last } }
    is $seen, 1_000_003, 'for ^Inf runs past the old 1M cap';
}

# -- laziness is reported and kept --------------------------------------------

ok ('a' .. *).is-lazy, 'Str: .is-lazy';
ok ('a' .. *).map(*.uc).is-lazy, 'Str: a .map pipe over it is lazy';
ok ('a' .. *).List.is-lazy, 'Str: .List is lazy';
ok (1.5 .. *).Seq.is-lazy, 'Rat: .Seq is lazy';
is (1 .. *).list.^name, 'List', '.list of an unbounded Int range is a List';
{
    my @a = 'a' .. *;
    ok @a.is-lazy, 'Str: array assignment stays lazy';
    is @a[2], 'c', 'Str: and indexes on demand';
}

# -- pipes, adaptors, scans ----------------------------------------------------

is ('a' .. *).map(*.uc).head(2).List, <A B>, 'Str: .map';
is ('a' .. *).grep(* ne 'b').head(2).List, <a c>, 'Str: .grep';
is ('a' .. *).pairs.head(2).List, (0 => 'a', 1 => 'b'), 'Str: .pairs';
is ('a' .. *).skip(1).head(2).List, <b c>, 'Str: .skip';
is ((1.5 .. *) Z ('a' .. *)).head(2).List, ((1.5, 'a'), (2.5, 'b')), 'zip of two unbounded ranges';
is ([\+] 1.5 .. *)[^3].List, (1.5, 4, 7.5), 'Rat: triangle reduction';
is ([\~] 'a' .. *)[^3].List, <a ab abc>, 'Str: triangle reduction';
is (1.5 .. *).AT-POS(2), 3.5, 'Rat: AT-POS';
is ('a' .. *).AT-POS(2), 'c', 'Str: AT-POS';

# -- for loops and slices of lazy lists ----------------------------------------

{
    my @got;
    for 'a' .. * { @got.push: $_; last if @got == 3 }
    is-deeply @got, [<a b c>], 'Str: for loop';
}
{
    my ($first, @rest) = 'a' .. *;
    is $first, 'a', 'list assignment takes the first element';
    is @rest[^2].List, <b c>, 'and the array the lazy rest';
    ok @rest.is-lazy, 'which stays lazy';
}
