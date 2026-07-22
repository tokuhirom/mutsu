use Test;

# .invert on a List held in a `$` scalar must invert the list's own Pair
# elements, not treat the (itemized) list as a single element. Previously
# `my $l = List.new(...); $l.invert` threw X::TypeCheck "got Array".

plan 7;

my $l = List.new('a' => (2, 3), 'b' => 17);
is-deeply $l.invert.List, (2 => 'a', 3 => 'a', 17 => 'b'),
    'invert on a $-held List with a list-valued pair';

my $p = (a => 1, b => 2);
is-deeply $p.invert.List, (1 => 'a', 2 => 'b'),
    'invert on a $-held list of scalar pairs';

# inline (non-itemized) list still works
is-deeply ('a' => (2, 3), 'b' => 17).invert.List, (2 => 'a', 3 => 'a', 17 => 'b'),
    'invert on an inline list of pairs';

my $single = ('x' => (2, 3),);
is-deeply $single.invert.List, (2 => 'x', 3 => 'x'),
    'invert on a $-held single list-valued pair';

# a Hash still inverts
my %h = a => 1, b => 2;
is-deeply %h.invert.sort.List, (1 => 'a', 2 => 'b'),
    'invert on a Hash';

# a non-pair element still throws X::TypeCheck with the right "got" type
dies-ok { (1, 2).invert }, 'a plain (non-pair) list still throws on invert';
my $bad = (1, 2);
throws-like { $bad.invert }, X::TypeCheck,
    'a $-held plain list throws X::TypeCheck on invert';
