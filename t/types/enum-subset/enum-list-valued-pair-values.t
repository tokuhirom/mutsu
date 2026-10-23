use Test;

plan 6;

# A colonpair enum member written with a list value keeps the whole list, in
# every spelling of the member list, and `.enums` hands the list out as such.
enum Sizes «
    :A5(148, 210)
    :A4(210, 297)
»;
is Sizes.enums<A4>.raku, '(210, 297)', 'quote-words member list with :key(a, b)';
is Sizes::A5.value.raku, '(148, 210)', 'the value of the member';

enum Paren (:a(1, 2), :b(3, 4));
is Paren.enums<b>.raku, '(3, 4)', 'parenthesized member list';

my @length;
for 'A4'.list {
    when Sizes.enums{.uc}:exists { @length = Sizes.enums{.uc}; }
}
is-deeply @length, [210, 297], 'assigning an enum list value to an array flattens it';

is Paren.enums.values.map(*.elems).sum, 4, 'all values are two-element lists';
is Paren.enums.^name, 'Map', '.enums is a Map';
