use Test;

# A native array's one-argument `new` stores the elements of any Iterable
# argument, itemized or not, including a Blob/Buf. An `Array` keeps an
# itemized argument whole. PDF's `t/io-util.t` builds
# `array[uint8].new($buf)`. Expected values are the `raku` (v2026.09) output.

plan 7;

is array[uint8].new(Buf[uint8].new(1, 2, 3)).join(','), '1,2,3', 'a Buf argument stores its bytes';
is array[uint8].new(blob8.new(4, 5)).join(','), '4,5', 'a Blob argument stores its bytes';
is array[int].new($[1, 2]).join(','), '1,2', 'an itemized Array argument stores its elements';
is array[int].new($(3, 4)).join(','), '3,4', 'an itemized List argument stores its elements';
is array[int].new([5, 6]).join(','), '5,6', 'a plain Array argument stores its elements';
is array[int].new(7).join(','), '7', 'a single value is one element';
is Array.new($[1, 2]).raku, '[[1, 2],]', 'an Array keeps an itemized argument whole';
