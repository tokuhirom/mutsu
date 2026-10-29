use Test;

# #15367: pushing onto an existing key extends the key's Array in place.
plan 7;

my %h;
%h.push: ('a' => $_) for ^2000;
is %h<a>.elems, 2000, 'repeated push keeps every value';
is %h<a>[0], 0, 'first value first';
is %h<a>[1999], 1999, 'last value last';

# An Array another holder shares still takes the push.
my %g;
%g.push: ('k' => 1);
%g.push: ('k' => 2);
my $alias = %g<k>;
%g.push: ('k' => 3);
is %g<k>.elems, 3, 'push after aliasing still appends';

my %a;
%a.append: ('k' => 1);
%a.append: ('k' => (2, 3));
%a.append: ('k' => 4);
is %a<k>.raku, q[$[1, 2, 3, 4]], 'append flattens into the stack';

my %p;
%p.push: ('k' => 1);
%p.push: ('k' => [2, 3]);
%p.push: ('k' => 4);
is %p<k>.elems, 3, 'push keeps an Array value as one element';
is %p<k>[1].elems, 2, 'the nested Array is intact';
