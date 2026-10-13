use Test;

plan 8;

# A BEGIN nested in a routine runs at compile time (ADR-0134, slice 2), even
# when it reads a variable whose type constraint names a type declared in the
# same inner scope (#10471). The variable's static cell lives at the unit's
# level, where that type does not exist, so it is untyped; the variable itself
# keeps its constraint.

my ($plain, $definite, $never-called);
sub never-called { my class K { }; my K $v; BEGIN $never-called = $v.raku }
is $never-called, 'K', 'the static state of a variable typed by an inner class';

sub stores {
    my class K { has $.n }
    my K $v;
    BEGIN $v = K.new(n => 3);
    $v.n;
}
is stores(), 3, 'a value the BEGIN stores is what the scope starts from';
is stores(), 3, '... on every entry';

sub resets { my class K { }; my K $v; BEGIN $v = Nil; $v.raku }
is resets(), 'K', 'Nil resets it to the type object';

sub definite {
    my class K { }
    my K:D $v = K.new;
    BEGIN $definite = $v.raku;
    $v.^name;
}
is $definite, 'K', 'a :D variable starts from its nominal type';
is definite(), 'K', '... and is initialized when the scope runs';

sub typed-array { my class K { }; my K @a; BEGIN @a.push(K.new); @a.elems }
is typed-array(), 1, 'an array typed by an inner class';

sub constraint-kept {
    my class K { }
    my K $v;
    BEGIN $plain = $v.raku;
    (try { $v = 42; 'assigned' }) // 'rejected';
}
is constraint-kept(), 'rejected', 'the variable keeps its type constraint';
