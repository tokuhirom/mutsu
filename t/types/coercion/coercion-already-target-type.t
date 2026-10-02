use Test;

# A coercion type only converts a value that is not already of its target
# type, looking through an item container (CSS::Properties'
# `List() :$ast` receives an itemized `$[…]`).

plan 5;

sub l(List() $l) { $l }
my $x = $[1, 2];
is l($x).elems, 2, 'an itemized Array is already a List';
is l([1, 2]).elems, 2, 'an Array is already a List';
is l($(1, 2)).elems, 2, 'an itemized List is kept';

sub s(Str() $s) { $s }
is s(5), '5', 'a value of another type is still coerced';

sub t(List() :$ast) { my @style; @style.append: .list with $ast; @style }
is t(:ast($x)).elems, 2, 'appending .list of the coerced value flattens it';
