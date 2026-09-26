use Test;

# A user `multi prefix:<->` overload for a class (Moneys' `Money`).
#
# `-Foo.new` must reach the overload: `-` is an identifier character only
# inside a name, so the operator is not "word-like" and needs no identifier
# boundary after it. And an argument none of the user candidates takes falls
# back to core negation of its *value*: `-$x` used to negate the lvalue
# wrapper and answer 0.

plan 6;

class Money { has $.amount }
multi sub prefix:<->(Money $m) { Money.new(amount => -$m.amount) }

is (-Money.new(amount => 3)).amount, -3, '-Type.new reaches the overload';
my $m = Money.new(amount => 4);
is (-$m).amount, -4, '-$var reaches the overload';

my $x = 5;
is -$x, -5, 'a variable no candidate takes gets core negation';
is prefix:<->($x), -5, 'the same through the routine name';
my $s = '2';
is -$s, -2, 'core negation numifies a Str';
sub seven { 7 }
is -seven(), -7, 'a call result no candidate takes';
