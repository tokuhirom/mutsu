use lib 't/lib';
use Test;
use AttrDefaultConstUser;

plan 6;

my $w = AttrDefaultConstUser.new;
is $w.a, 42, 'imported sub in an attribute default';
is $w.b, 'ab', 'imported constant in an attribute default is its value, not its name';
is $w.c.raku, 'E::EA', 'imported enum value in an attribute default';
is-deeply $w.d, [1, 2], 'imported list constant in an array attribute default';
is-deeply $w.m, (1, 2), 'the same constant in a method body';
is AttrDefaultConstUser.new(b => 'x').b, 'x', 'an explicit argument still wins';
