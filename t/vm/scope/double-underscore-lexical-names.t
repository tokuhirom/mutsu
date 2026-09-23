use Test;

# A user lexical whose name starts with `_` or `__` is an ordinary identifier.
# `my $__x = 5` used to be parsed as `my $_` followed by an assignment to a
# variable named `_x`, so `$__x` read back as Nil.

plan 12;

{
    my $__t0 = 5;
    is $__t0 + 1, 6, 'my $__t0 reads back its value';
    is "$__t0", '5', '$__t0 interpolates';
}
{
    my ($__a, $__b) = 1, 2;
    is $__a + $__b, 3, 'my ($__a, $__b) list declaration';
}
{
    our $__o = 3;
    is $__o, 3, 'our $__o';
}
{
    my @__x = 1, 2;
    is-deeply @__x, [1, 2], '@__x round-trips';
    my %__x = a => 1;
    is-deeply %__x, {a => 1}, '%__x round-trips';
    my &__g = { 7 };
    is __g(), 7, '&__g round-trips';
}
{
    my $__c = 5;
    my $cl = { $__c + 1 };
    $__c = 10;
    is $cl(), 11, 'closure sees later writes to $__c';
}
{
    my $_x = 2;
    is $_x, 2, 'single-underscore $_x is still an ordinary lexical';
    my $_ä = 3;
    is $_ä, 3, 'a non-ASCII letter after $_ makes an identifier';
}
{
    $_ = 4;
    is $_, 4, 'plain $_ is still the topic';
    sub f($__p) { $__p * 2 }
    is f(4), 8, 'a $__p parameter binds';
}
