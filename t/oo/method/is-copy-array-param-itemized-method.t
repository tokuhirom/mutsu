use Test;

plan 5;

class C {
    method shifted(@l is copy) { @l.shift; @l.map({ 1 }).elems }
    method popped(@l is copy)  { @l.pop;   @l.map({ 1 }).elems }
    method raw(@l is copy)     { @l.shift; @l.raku }
}
my $c = C.new;

is $c.shifted($[1]), 0, 'an itemized one-element array, shifted empty, maps zero times';
is $c.popped($[1]), 0, 'same after a pop';
is $c.raku, 'C.new', 'sanity';
is $c.raw($[1]), '[]', 'the copy is a plain Array, not an itemized holder';
is $c.shifted($[1, 2]), 1, 'two elements, one shifted';
