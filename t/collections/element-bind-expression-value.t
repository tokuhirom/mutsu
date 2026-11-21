use Test;

# #18260: the value of an element bind expression is the bound value itself,
# not an itemized `$(...)` of it. The element was already stored bare.

plan 17;

my %h;
is (%h<a> := (1, 2).Seq).raku, '(1, 2).Seq', 'hash element bind of a Seq';
my @a;
is (@a[0] := (1, 2).Seq).raku, '(1, 2).Seq', 'array element bind of a Seq';
is (%h<b> := (1, 2)).raku, '(1, 2)', 'hash element bind of a List';
is (%h<c> := [1, 2]).raku, '[1, 2]', 'hash element bind of an Array';
is (%h<d> := $[1, 2]).raku, '$[1, 2]', 'an itemized value stays itemized';
is %h<a>.raku, '(1, 2).Seq', 'the stored element is bare';

# A routine returning `%cache{$k} := $seq` hands back a list, not one item
# (Backtrace::Files' path-lines).
my %cache;
sub lines-of($k) { %cache{$k} // (%cache{$k} := (1, 2, 3).Seq) }
my @l = lines-of('x');
is @l.elems, 3, 'first call: the bound Seq flattens into the array';
@l = lines-of('y');
is @l.elems, 3, 'again for another key';

# Chained and multi-dimensional subscripts.
sub chained { my %n; %n<a><b> := (1, 2) }
is chained().raku, '(1, 2)', 'a chained element bind answers the value';
my %m;
sub multi { %m{1;2} := (5, 6) }
is multi().raku, '(5, 6)', 'a multi-dimensional element bind answers the value';

# An attribute element.
class C { has %.h; method m { %!h<a> := (1, 2) } }
is C.new.m.raku, '(1, 2)', 'an attribute element bind answers the value';

# Binding a variable's container still writes through.
my $x = 5;
(%h<e> := $x) = 7;
is $x, 7, 'assigning to a hash element bind of $x writes $x';
(@a[1] := $x) = 9;
is $x, 9, 'assigning to an array element bind of $x writes $x';
is (%h<f> := $x).raku, '9', 'the value of a bind to $x';

# A bind statement is not sunk: the bound Seq stays unconsumed (rakudo).
my %s;
%s<k> := (4, 5).Seq;
is %s<k>.elems, 2, 'a Seq bound by a statement is still readable';
my @s;
@s[0] := (6, 7).Seq if True;
is @s[0].elems, 2, 'also under a statement modifier';
my $mapped = 0;
%s<m> := (1, 2).Seq.map({ $mapped++; $_ });
is $mapped, 0, 'a bound map Seq is not iterated by the statement';
