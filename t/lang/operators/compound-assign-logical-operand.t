use Test;

plan 9;

# `A || B //= v` parses as `(A || B) //= v`: the operator yields the selected
# operand's container, so the compound assignment writes to that operand.
my $b = 0;
(0 || $b) //= 6;
is $b, 0, '//= on a defined container leaves it';

my $u;
(0 || $u) //= 6;
is $u, 6, '(0 || $u) //= 6 assigns the picked operand';

my %h;
(0 || %h<j>) //= 8;
is-deeply %h, {j => 8}, 'a hash element picked by ||';

my $x = 3;
(1 && $x) += 2;
is $x, 5, '(1 && $x) += 2 writes to the right operand';

my $first = 1;
my $second = 10;
($first || $second) += 5;
is $first, 6, 'the left operand when truthy';
is $second, 10, 'and not the right';

my %meths;
sub lookup(\name) { 0 || %meths{name} //= do { (name.uc,) } }
is lookup('ab')[0], 'AB', 'the idiom of CSS::Box.can';
is %meths<ab>[0], 'AB', 'the element was stored';

my @a;
(0 || @a[1]) //= 4;
is-deeply @a, [Any, 4], 'an array element';
