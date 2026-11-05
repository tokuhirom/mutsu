use v6;
use Test;

# Assigning to a non-rw public scalar attribute whose value is an Array, Hash
# or Buf replaces that object's contents (the accessor hands back the object
# and `=` on it is its STORE); an Int still dies X::Assignment::RO.
# Net::BGP's `self.buffer = buf8.new` on `has buf8:D $.buffer`.

plan 12;

class C {
    has buf8:D $.buffer = buf8.new(1);
    has $.arr = [1];
    has Hash $.h = {};
    has $.n = 1;
    method set-buffer { self.buffer = buf8.new(7, 8); self.buffer.elems }
    method clear-buffer { $.buffer = buf8.new; $.buffer.elems }
    method set-n { self.n = 2; self.n }
}

my $c = C.new;
is $c.set-buffer, 2, 'self.buffer = ... replaces the Buf contents';
is $c.buffer.list, (7, 8), 'the new contents are visible';
is $c.clear-buffer, 0, '$.buffer = (empty Buf) clears the stored Buf';

my $alias = $c.buffer;
$c.buffer = buf8.new(5);
is $alias.list, (5,), 'an alias of the stored Buf sees the replacement';

$c.arr = [2, 3];
is $c.arr.list, (2, 3), 'Array attribute replaced through the accessor';
$c.h = {a => 1};
is $c.h<a>, 1, 'Hash attribute replaced through the accessor';

throws-like { $c.set-n }, X::Assignment::RO, 'an Int attribute is still read-only';
throws-like { $c.n = 5 }, X::Assignment::RO, 'also from outside';

my $other = C.new;
is $other.buffer.list, (1,), 'another instance is unaffected';
$c.buffer = 9;
is $c.buffer.list, (9,), 'a single element is stored as a one-element Buf (Buf.STORE)';
$c.buffer = (3, 4);
is $c.buffer.list, (3, 4), 'a list is stored element by element';
is $c.arr.elems, 2, 'array unchanged by the others';
