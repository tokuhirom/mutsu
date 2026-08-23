use Test;

plan 2;

class Foo {
    submethod bar { &?ROUTINE.^name }
    method baz { &?ROUTINE.^name }
}

is Foo.bar, 'Submethod', '&?ROUTINE.^name identifies a submethod';
is Foo.baz, 'Method', '&?ROUTINE.^name still identifies a regular method';
