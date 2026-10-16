use Test;

plan 3;

# `is rw` on a class declared in expression position is a class trait,
# not a parent class named `rw`.

my $x = class D is rw { has $.a };
is $x.^name, 'D', 'class expression with is rw';
my $o = $x.new(a => 1);
$o.a = 5;
is $o.a, 5, 'attribute is writable through the accessor';

is (class E is rw { }).^name, 'E', 'parenthesised form still works';
