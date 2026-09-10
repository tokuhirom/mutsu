use Test;

# Nil assigned to a typed array element reverts to the element type's default,
# i.e. the element type object itself (Raku container semantics). A `:D`
# element type has no default type object, so it dies.

plan 13;

{
    my Bool @a = Nil;
    is @a.raku, 'Array[Bool].new(Bool)', 'bare Nil initializer -> element type object';
    is @a.elems, 1, 'bare Nil initializer yields one element';
}

{
    my Int @a = Nil;
    is @a.raku, 'Array[Int].new(Int)', 'Int typed array Nil -> Int type object';
}

{
    my Bool @a = (Nil,);
    is @a.raku, 'Array[Bool].new(Bool)', 'single-element list Nil -> type object';
}

{
    my Bool @a = True, Nil, False;
    is @a.raku, 'Array[Bool].new(Bool::True, Bool, Bool::False)',
        'Nil in the middle of a typed list reverts to the type object';
}

{
    my Int @a = 1, Nil, 3;
    is @a.raku, 'Array[Int].new(1, Int, 3)', 'Nil hole in Int list';
}

{
    my Bool @a;
    @a = Nil;
    is @a.raku, 'Array[Bool].new(Bool)', 'reassigning Nil to a typed array';
}

{
    my Bool @a = (True,);
    @a = Nil;
    is @a.raku, 'Array[Bool].new(Bool)', 'reassigning Nil replaces populated array';
}

{
    my Bool @a;
    is @a.raku, 'Array[Bool].new()', 'no-initializer typed array stays empty';
}

{
    # Untyped array keeps the Any default.
    my @a = Nil;
    is @a.raku, '[Any]', 'untyped array Nil -> Any';
}

# A definite element type has no default type object -> dies.
dies-ok { my Int:D @a = Nil }, 'Nil into :D typed array dies';

# An explicit non-Nil type object that fails the constraint still dies.
dies-ok { my Bool @a = Int }, 'wrong type object into typed array dies';
dies-ok { my Bool @a = Any }, 'Any type object into Bool array dies';
