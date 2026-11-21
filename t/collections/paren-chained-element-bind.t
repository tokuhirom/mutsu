use Test;

# #18499: a chained element bind (`%h<a><b> := v`) parses in expression
# position -- in parentheses, as an initializer, as a routine's value -- not
# only as a statement, and binds the autovivified element.

plan 8;

my %h;
is (%h<a><b> := 1), 1, 'parenthesized chained hash bind answers the value';
is-deeply %h, {a => ${b => 1}}, 'and binds the autovivified element';

my @a = [1, 2], [3];
is-deeply (@a[0][1] := (3, 4)), (3, 4), 'parenthesized chained array bind';
is-deeply @a[0][1], (3, 4), 'the element holds the bound List';

my %n;
my $v = (%n<a><b><c> := 7);
is $v, 7, 'a three-level bind as an initializer';
is %n<a><b><c>, 7, 'binds the deepest element';

sub chained-bind { my %c; %c<x><y> := (1, 2) }
is-deeply chained-bind(), (1, 2), 'a chained bind as the value of a routine';

my %p;
my @r = (%p<k><l> := (5, 6));
is @r.elems, 2, 'the bound list flattens into an array assignment';
