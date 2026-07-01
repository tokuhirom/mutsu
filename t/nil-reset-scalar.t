use Test;

plan 8;

# Reassigning Nil to an untyped scalar container resets it to the default type
# object, Any (Raku container semantics). Typed scalars reset to their own type.

{
    my $x = 5;
    $x = Nil;
    ok $x === Any, 'reassigning Nil to an untyped scalar yields Any';
    nok $x.defined, 'the reset value is undefined';
}

{
    # xor/^^ of two true values yields Nil, which resets the scalar to Any.
    my $x = 42;
    $x ^^= 15;
    ok $x === Any, '^^= with two true arguments yields Nil -> Any';
    $x ^^= 'xyzzy';
    is $x, 'xyzzy', "^^= doesn't permanently falsify scalars";
}

{
    my $x = 42;
    $x xor= 15;
    ok $x === Any, 'xor= with two true arguments yields Nil -> Any';
}

{
    # A typed scalar resets to its own type object, not Any.
    my Int $y = 5;
    $y = Nil;
    ok $y === Int, 'reassigning Nil to a typed scalar yields the type object';
}

{
    # `@`/`%` element assignment is unaffected: it may hold Nil directly.
    my @a = 1, 2;
    @a[0] = Nil;
    ok !@a[0].defined, 'array element assigned Nil is undefined';
    is @a.elems, 2, 'array keeps its shape';
}
