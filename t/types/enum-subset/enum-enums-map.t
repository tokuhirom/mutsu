use Test;

# `Enumeration.enums` is an immutable Map of each value's name to its value,
# built natively (#16978) the way `Map.new(|pairs)` would build it.

plan 7;

enum E <a b c>;
isa-ok E.enums, Map, 'enums is a Map';
is-deeply E.enums.sort.List, (a => 0, b => 1, c => 2), 'every name and value';
is E.enums<b>.VAR.^name, 'Int', 'values are bare, not item containers';
is a.enums.elems, 3, 'an enum value answers for its type';

enum P (:A4(210, 297), :B5(176, 250));
is-deeply P.enums<A4>, (210, 297), 'a list value stays the list';

is-deeply Bool.enums.sort.List, (False => 0, True => 1), 'Bool.enums';

dies-ok { my $m = E.enums; $m<a> = 5 }, 'the Map is immutable';
