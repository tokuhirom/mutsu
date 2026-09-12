use v6;
use Test;

plan 9;

# A CLASS-LEVEL attribute (`our $.x` / `my $.x`) may be BOUND with `:=` as well
# as assigned with `=`. Binding makes the accessor hand back the very container
# on the right-hand side, so a later mutation of that container is visible
# through the accessor. This is the shape Math::Symbolic's
# `Language.rakumod` uses (`our @.operations := @operations;`).
# Every assertion below was checked against rakudo itself.

{
    my @source = 1, 2;
    class WithArray {
        our @.operations := @source;
    }
    @source.push(3);
    is WithArray.operations, [1, 2, 3],
        'our @.x := @c makes the accessor see later pushes to @c';
}

{
    my %source = a => 1;
    class WithHash {
        our %.by-name := %source;
    }
    %source<b> = 2;
    is WithHash.by-name.sort.map(*.kv.join('=')).join(','), 'a=1,b=2',
        'our %.x := %c binds the hash container itself';
}

{
    my $source = 5;
    class WithScalar {
        our $.n := $source;
    }
    is WithScalar.n, 5, 'our $.x := $c binds a scalar';
}

{
    my @source = 1, 2;
    class MyScoped {
        my @.x := @source;
    }
    @source.push(3);
    is MyScoped.x, [1, 2, 3], 'my @.x := @c binds the same way `our` does';
}

# The bind must not disturb the `=` spelling that was already there.
{
    my @source = 1, 2;
    class Assigned {
        our @.x = @source;
    }
    is Assigned.x, [1, 2], 'our @.x = @c still initializes from the list';
}

# A PER-INSTANCE attribute has no container yet at declaration time -- its
# storage is built by the constructor -- so rakudo refuses `:=` there outright.
{
    my $err;
    try {
        EVAL 'class HasBind { has $.x := 1 }';
        CATCH { default { $err = $_ } }
    }
    ok $err.defined, 'has $.x := ... is refused';
    is $err.message, 'Cannot use := to initialize an attribute',
        'and refused with rakudo\'s own message';
    ok $err ~~ X::Comp::AdHoc, 'as an X::Comp::AdHoc';
}

{
    my $err;
    try {
        EVAL 'class HasBindArray { has @.x := (1, 2) }';
        CATCH { default { $err = $_ } }
    }
    is $err.message, 'Cannot use := to initialize an attribute',
        'the array spelling is refused identically';
}

done-testing;
