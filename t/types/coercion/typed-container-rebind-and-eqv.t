use Test;

# A `:=` rebind is checked against the variable's DECLARED element type, not
# against the type of whatever it is bound to right now; element operations
# follow the currently bound container. `eqv` compares the container's type
# parameterization too (#9852).

plan 26;

# --- rebinding an untyped @/% variable ---
{
    my @a := Array[Int].new(1, 2, 3);
    @a := Array[Str].new("a", "b");
    is @a.raku, 'Array[Str].new("a", "b")', 'untyped @a rebinds to a differently typed Array';
    throws-like { @a[0] = 1 }, X::TypeCheck, 'element store follows the newly bound Array[Str]';
    @a := [1, "x"];
    lives-ok { @a.push(2.5) }, 'rebinding to an untyped Array drops the earlier element type';
}
{
    my %h := Hash[Int].new;
    %h := Hash[Str].new;
    is %h.WHAT.raku, 'Hash[Str]', 'untyped %h rebinds to a differently typed Hash';
    throws-like { %h<a> = 1 }, X::TypeCheck, 'hash element store follows the bound Hash[Str]';
}

# --- a typed declaration keeps constraining later rebinds ---
{
    my Cool @c := Array[Int].new(1);
    throws-like { @c[0] = "x" }, X::TypeCheck, 'element store obeys the bound Array[Int]';
    @c := Array[Str].new("a");
    is @c.raku, 'Array[Str].new("a")', 'typed @c rebinds to another conforming Array';
    throws-like { @c := Array[Any].new(1) }, X::TypeCheck::Binding,
        'typed @c still rejects a non-conforming Array after a rebind';
    my Numeric @n := Array[Int].new(1);
    @n := Array[Rat].new(1.5);
    is @n.raku, 'Array[Rat].new(1.5)', 'Numeric @n rebinds Array[Int] then Array[Rat]';
    throws-like { @n := Array[Str].new("q") }, X::TypeCheck::Binding,
        'Numeric @n rejects Array[Str]';
}

# --- scoping of the declared type ---
{
    my Int @t;
    { my @t := Array[Str].new("x"); is @t.raku, 'Array[Str].new("x")', 'inner untyped @t binds freely' }
    throws-like { @t := Array[Str].new("y") }, X::TypeCheck::Binding,
        'outer typed @t is unaffected by an inner same-named bind';
    my @r;
    for ^2 {
        my @l := Array[Int].new(1);
        @l := Array[Str].new("z");
        @r.push: @l.raku;
    }
    is-deeply @r, ['Array[Str].new("z")' xx 2], 'loop re-declaration rebinds on every iteration';
}

# --- rebinding a captured variable from a closure / sub ---
{
    my @a := Array[Int].new(1);
    my &f = sub { @a := Array[Str].new("s") };
    f();
    is @a.raku, 'Array[Str].new("s")', 'a closure rebinds a captured untyped @a';
    { @a := Array[Num].new(1e0) }
    is @a.raku, 'Array[Num].new(1e0)', 'a bare block rebinds it too';
    my %g;
    my sub hh { %g := Hash[Int].new; %g := Hash[Str].new }
    hh();
    is %g.WHAT.raku, 'Hash[Str]', 'a sub rebinds a captured untyped %g twice';
    my Int @ti;
    my &bad = sub { @ti := Array[Str].new("s") };
    throws-like { bad() }, X::TypeCheck::Binding, 'a closure rebind still checks the declared type';
}

# --- eqv compares the type parameterization ---
{
    my @b := Array[Int].new(1, 2, 3);
    my @c = 1, 2, 3;
    nok @b eqv @c, 'Array[Int] eqv Array is False';
    my Int @d = 1, 2, 3;
    nok @d eqv @c, 'my Int @d eqv untyped Array is False';
    ok @d eqv Array[Int].new(1, 2, 3), 'my Int @d eqv Array[Int] is True';
    nok [Array[Int].new(1)] eqv [[1],], 'nested typed Array is compared by type too';
    nok infix:<eqv>(Array[Int].new(1), [1]), 'the routine form agrees';
    nok Array[Str].new eqv Array[Int].new, 'empty arrays of different element types differ';
    my Int:D @x = 1;
    nok @x eqv Array[Int].new(1), 'Int:D and Int parameterizations differ';
    nok Hash[Int].new((a => 1)) eqv {a => 1}, 'Hash[Int] eqv Hash is False';
    my int @n = 1;
    nok @n eqv [1], 'native array eqv Array is False';
}
