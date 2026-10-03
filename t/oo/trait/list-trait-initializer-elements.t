use Test;

# `my @l is List = ...` keeps the initializer's elements as they are: an Array
# or Hash element is the container itself, not a Scalar-wrapped copy. Found in
# JSON::Fast::Hyper's test, which round-trips `my @list is List = 1, 2, 3,
# @array, %hash` and compares with is-deeply.

plan 7;

my @array = <a b>;
my %hash  = :1x;
my @list is List = 1, @array, %hash;
is @list.raku, '(1, ["a", "b"], {:x(1)})', 'elements are not itemized';
is @list[1].VAR.^name, 'Array', 'Array element is the Array itself';
is @list[2].VAR.^name, 'Hash', 'Hash element is the Hash itself';
is @list.^name, 'List', 'the variable holds a List';
my @flat is List = @array;
is @flat.raku, '("a", "b")', 'a single Array initializer is flattened';
my @one is List = 5;
is @one.raku, '(5,)', 'a single scalar initializer';
my @seq is List = (1..3).map(* + 1);
is @seq.raku, '(2, 3, 4)', 'a Seq initializer';
