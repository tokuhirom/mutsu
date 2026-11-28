use Test;

# #19318: ++ and -- (prefix and postfix) on a multi-dimensional subscript.
plan 12;

my @a;
@a[0;1]++;
is-deeply @a[0;1], 1, 'postfix ++ creates and steps the element';
--@a[0;1];
is-deeply @a[0;1], 0, 'prefix -- steps it back';
is ++@a[0;1], 1, 'prefix ++ answers the new value';
is @a[0;1]--, 1, 'postfix -- answers the old value';
is @a[0;1], 0, 'and stepped it';

my @b[2;2];
@b[1;1]++;
@b[1;1]++;
is @b[1;1], 2, 'a shaped array element';

my $i = 0;
@a[$i++;0]++;
is $i, 1, 'a subscript with a side effect is evaluated once';
is @a[0;0], 1, 'and steps the element it named';

my %h{Str;Int};
%h{"a";1} = 1;
%h{"a";1}++;
is %h{"a";1}, 2, 'a shaped hash element';
is %h{"a";1}++, 2, 'postfix ++ on a shaped hash answers the old value';
is ++%h{"a";1}, 4, 'prefix ++ on a shaped hash answers the new value';
%h{"b";2}--;
is %h{"b";2}, -1, 'an absent shaped hash entry is created';
