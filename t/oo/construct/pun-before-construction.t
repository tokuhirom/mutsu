use v6;
use Test;

plan 6;

# `R.^pun` must not leave the role registered as a plain class: a later `R.new`
# then built an instance without the role markers, whose `.WHAT` was not
# `R.^pun`. Game::Entities keys its component registry by `.^pun` for a role
# type object and by `.WHAT` for an instance, so the order in which a test
# first touched the role decided whether components were found.

role Aging { has Int $.age }

my $pun = Aging.^pun;
my $instance = Aging.new(age => 1);

ok $instance.WHAT === $pun, 'an instance built after .^pun has the pun as its type';
ok $instance ~~ Aging, 'the instance still does the role';
is $instance.age, 1, 'the attribute is set';
is $pun.new(age => 3).age, 3, 'the pun itself constructs';
ok Aging.new(age => 2).WHAT === Aging.^pun, 'every construction agrees with .^pun';

my %by-type{Mu};
%by-type{Aging.^pun} = 'found';
is %by-type{Aging.new(age => 4).WHAT}, 'found', 'an object hash keyed by the pun finds an instance type';
