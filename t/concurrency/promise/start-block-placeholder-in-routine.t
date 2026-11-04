use Test;

plan 6;

# A `start` block (and `do`/`try`/`once`) takes no signature, so a placeholder
# used directly in it is rejected when the unit is compiled, in a routine body
# as much as in the mainline (#16303). EVAL'd text raises the same exception.

throws-like { EVAL 'start { $^c }' }, X::Placeholder::Block,
    'mainline start block placeholder is rejected';
throws-like { EVAL 'sub f { start { $^c } }; 1' }, X::Placeholder::Block,
    'start block placeholder inside a routine body is rejected at compile time, without calling it';
throws-like { EVAL 'sub f { do { $^c } }; 1' }, X::Placeholder::Block,
    'do block placeholder inside a routine body is rejected at compile time';
throws-like { EVAL 'sub f { try { $:c } }; 1' }, X::Placeholder::Block,
    'try block named placeholder inside a routine body is rejected at compile time';

# What stays legal: a bare block and a pointy block take their placeholders,
# and a method has an implicit *%_.
is (EVAL 'sub g { my $b = { $^x * 2 }; $b(21) }; g()'), 42,
    "a bare block inside a routine still takes its placeholder";
is (EVAL 'class A { method m { await start { %_.elems } } }; A.new.m'), 0,
    'the implicit slurpy hash inside a start block of a method is legal';
