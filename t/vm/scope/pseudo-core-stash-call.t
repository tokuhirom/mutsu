use Test;

# A routine bound into the setting's stash is what a call of the core name
# reaches, unless the program declares its own routine of that name (#18779).

plan 6;

is none(1, 2).^name, 'Junction', 'before the bind, none() is the builtin';

sub f1(*@a) { "f1:" ~ @a.elems }
&CORE::none := &f1;
is none(1, 2), 'f1:2', '&CORE::none := &f makes none() call it';
is &none.name, 'f1', '... and &none is it';

sub inner() { none(3) }
is inner(), 'f1:1', 'a call from inside a routine sees the bind too';

sub f2(*@a) { "f2" }
CORE::<&one> := &f2;
is one(1), 'f2', 'CORE::<&one> := &f binds the same way';

{
    sub any(*@a) { "mine" }
    sub f3(*@a) { "f3" }
    &CORE::any := &f3;
    is any(1), 'mine', 'a routine the program declares still wins';
}
