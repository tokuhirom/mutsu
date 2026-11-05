use Test;

# A Callable subscript (`@a[-> $n { ... }]`, `@a[$i % *]`) is an ordinary call:
# what it throws, or a Failure it returns, surfaces instead of reading as Nil.

plan 8;

my $i = 3;
my @empty;
my @a = 1, 2, 3;

throws-like { @a[-> $n { die "boom" }] }, X::AdHoc, message => 'boom', 'a die inside the index closure';
throws-like { my %h; %h[-> $n { die "boom" }] }, X::AdHoc, message => 'boom', 'on a Hash';
throws-like { (1, 2, 3)[-> $n { die "boom" }] }, X::AdHoc, message => 'boom', 'on a List';
throws-like { @empty[$i % *] }, X::Numeric::DivideByZero, 'a Failure from WhateverCode over an empty array';
throws-like { @empty[-> $n { $i % $n }] }, X::Numeric::DivideByZero, 'same through a pointy block';
is @a[* - 1], 3, 'a normal WhateverCode index still works';
is @a[-> $n { $n - 2 }], 2, 'and so does a pointy block';
is (1..5)[* - 2], 4, 'on a Range';
