use Test;

# An element bound to a value or to an `@`/`%` variable holds that object
# itself, with no Scalar container around it (rakudo's BIND-KEY/BIND-POS,
# #18566): it reads and iterates as the object, `.VAR` answers the object's
# type, and `=` to the element dies instead of reassigning the source.

plan 27;

# `%h<k> := @s`, one level.
{
    my @s = 1, 2;
    my %h;
    %h<a> := @s;
    is %h<a>.raku, '[1, 2]', 'reads as the Array';
    is %h<a>.VAR.^name, 'Array', '.VAR is the Array, not a Scalar';
    is (gather for %h<a> { take $_ }).join(','), '1,2', 'for iterates its elements';
    throws-like { %h<a> = 4 }, X::AdHoc, message => 'Cannot assign to an immutable value',
        '= to the element dies';
    is @s.raku, '[1, 2]', 'and does not touch the source';
    %h<a>.push(3);
    is @s.raku, '[1, 2, 3]', 'a mutator reaches the shared Array';
}

# `%h<a><b> := @s`, chained.
{
    my @s = 1, 2;
    my %g;
    %g<a><b> := @s;
    is %g<a><b>.raku, '[1, 2]', 'chained: reads as the Array';
    is %g<a><b>.VAR.^name, 'Array', 'chained: .VAR is the Array';
    is (gather for %g<a><b> { take $_ }).join(','), '1,2', 'chained: for iterates its elements';
    throws-like { %g<a><b> = 9 }, X::AdHoc, 'chained: = dies';
    @s.push(3);
    is %g<a><b>.raku, '[1, 2, 3]', 'chained: the Array is shared';
}

# `@a[0] := @s`.
{
    my @s = 1, 2;
    my @a;
    @a[0] := @s;
    is @a[0].VAR.^name, 'Array', 'positional: .VAR is the Array';
    is (gather for @a[0] { take $_ }).join(','), '1,2', 'positional: for iterates its elements';
    throws-like { @a[0] = 5 }, X::AdHoc, 'positional: = dies';
    is @s.raku, '[1, 2]', 'positional: the source is untouched';
    ok @a[0] =:= @s, 'positional: the element is =:= to the Array';
}

# Value binds.
{
    my %h;
    %h<a> := [1, 2];
    is %h<a>.VAR.^name, 'Array', 'a bound Array literal has no Scalar';
    is (gather for %h<a> { take $_ }).join(','), '1,2', 'and iterates';
    %h<b> := 42;
    is %h<b>.VAR.^name, 'Int', 'a bound Int has no Scalar';
    my @a;
    @a[0] := (1, 2);
    is (gather for @a[0] { take $_ }).join(','), '1,2', 'a bound List iterates';
    is @a[0].VAR.^name, 'List', 'and its .VAR is the List';
}

# An `=` share and a scalar bind keep their containers.
{
    my @r = 3, 4;
    my @aoa;
    @aoa[0] = @r;
    @aoa[0] = 9;
    is @aoa.raku, '[9]', 'an =-shared element is still reassignable';
    my $x = 1;
    my %h;
    %h<x> := $x;
    is %h<x>.VAR.^name, 'Scalar', 'an element bound to a $ variable is its Scalar';
    %h<x> = 5;
    is $x, 5, 'and writes through';
}

# A ternary RHS binds what its arm yields: an anonymous `my $` stays a Scalar.
{
    my %e;
    my $c = True;
    %e<x> := $c ?? my $ !! [];
    is %e<x>.VAR.^name, 'Scalar', 'a ternary arm keeps an anonymous Scalar';
    %e<x> = 5;
    is %e<x>, 5, 'and it is writable';
    %e<y> := !$c ?? my $ !! [];
    is %e<y>.VAR.^name, 'Array', 'the other arm binds its Array';
}
