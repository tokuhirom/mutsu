use Test;

plan 10;

# Routine parameters are read-only by default in Raku. This must hold for
# *method* parameters too (not just subs) — assigning to or mutating a plain
# `$` method parameter dies with X::Assignment::RO.

class C {
    method assign($x)      { $x = 5 }
    method subst($_)       { s/c// }
    method incr($x)        { $x++ }
    method rw($x is rw)    { $x = 5; $x }
    method copy($x is copy) { $x = 5; $x }
    method arr(@a)         { @a.push(9); @a }
    method hsh(%h)         { %h<k> = 1; %h }
    method read($x)        { $x * 2 }
}

dies-ok { C.new.assign(3) },  'method $x = 5 dies (X::Assignment::RO)';
dies-ok { C.new.subst('ccc') }, 's/// on method $_ param dies';
dies-ok { C.new.incr(3) },    'method $x++ dies';

throws-like { C.new.assign(3) }, X::Assignment::RO,
    'method param assignment throws X::Assignment::RO';

is C.new.rw(my $a = 1), 5,   'is rw method param is writable';
is C.new.copy(7), 5,         'is copy method param is writable';
is-deeply C.new.arr([1, 2]), [1, 2, 9], '@ method param container is mutable';
is-deeply C.new.hsh(%(a => 1)).sort, (a => 1, k => 1), '% method param is mutable';
is C.new.read(21), 42,       'reading a method param works';

# The read-only marking must not leak past the method: a caller variable of the
# same name stays writable afterwards.
{
    my $x = 1;
    try C.new.assign(99);  # swallow the X::Assignment::RO
    $x = 2;
    is $x, 2, 'caller $x stays writable after a method with a $x param';
}
