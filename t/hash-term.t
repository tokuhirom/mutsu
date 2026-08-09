use Test;

plan 3;

my %h = a => 1, b => 2;
%h = hash;
is %h.elems, 0, 'bare hash term produces an empty hash';

is hash.raku, '{}', 'hash.raku stringifies the empty hash literal';

is hash(a => 1).raku, '{:a(1)}', 'hash(...) with args still works';

done-testing;
