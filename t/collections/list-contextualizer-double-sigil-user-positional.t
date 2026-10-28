use Test;

# `@@x` is `@(@x)`: a user `Positional` bound to `@x` answers its `.list`; an
# ordinary Array is its own list context (annotations' Buffer, #15286).

plan 4;

class Buf1 does Positional {
    has @!b = 1..3;
    method elems { 3 }
    method AT-POS(Int:D $p) is raw { @!b[$p] }
    method list(::?CLASS:D:) { @!b }
}

my @buffer := Buf1.new;
is @@buffer, (1, 2, 3), '@@x on a bound user Positional is its list';
is @@buffer.elems, 3, 'and has the list\'s elements';

my @a = 1, 2;
@@a.push: 3;
is @a, (1, 2, 3), '@@x on an Array is the Array itself';
is @@a.WHAT, Array, 'and stays an Array';
