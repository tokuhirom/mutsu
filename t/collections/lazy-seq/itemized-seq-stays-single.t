use Test;

plan 7;

# A Seq held in a `$` container is itemized: list operations keep it as one
# element, exactly like an itemized List (Format::Lisp joins the directive
# results with `join('', map { ...; $text }, @directives)`).
my $t = (1, 2).Seq;
is join("-", $t), '1 2', 'join does not flatten a $-held Seq';
is (1, $t).flat.elems, 2, '.flat leaves it as one element';
is join("-", (1, $t)), '1-1 2', 'inside a list';
my $s = (1, 2).Seq;
is join("-", map { my $q = $s; $q }, ^2), '1 2-1 2', 'returned from a map block';
my $l = (1, 2);
is join("-", $l), '1 2', 'an itemized List, for comparison';
is join(",", (3, 4).Seq), '3,4', 'a bare Seq still flattens';
my @a = (1, 2).Seq;
is @a.elems, 2, 'and assigning a bare Seq to an array spreads it';
