use Test;

# #9730: a `$`-sigil `:=` bind whose source is a bare TYPE OBJECT (a class or a
# core type named by a bareword, not a variable) leaves the target with no
# Scalar container at all -- exactly like binding to an immutable literal
# (`$x := 5`) -- so a later whole-value assignment must be refused. Unlike the
# literal case, rakudo's own wording for this shape names the type: "assign
# requires a concrete object (got a IB type object instead)", not the generic
# "Cannot assign to an immutable value".

plan 7;

class IB { }

{
    my $s;
    $s := IB;
    throws-like { $s = 1 }, X::AdHoc,
        'assigning a scalar bound to a class type object dies',
        message => 'assign requires a concrete object (got a IB type object instead)';
}

{
    my $s;
    $s := Int;
    throws-like { $s = 1 }, X::AdHoc,
        'assigning a scalar bound to a core type object dies',
        message => 'assign requires a concrete object (got a Int type object instead)';
}

{
    my $s := IB;
    throws-like { $s = 1 }, X::AdHoc,
        'the declaration spelling dies the same way',
        message => 'assign requires a concrete object (got a IB type object instead)';
}

{
    # A literal bind is still the OTHER, generic wording -- the two must not
    # collide into one kind.
    my $s;
    $s := 1;
    throws-like { $s = 2 }, X::AdHoc,
        'a literal bind keeps the generic immutable-value wording',
        message => 'Cannot assign to an immutable value';
}

{
    # A rebind to a NAMED variable clears the type-object mark and restores
    # ordinary write-through aliasing (#9277's exception, extended to this
    # kind).
    my $s;
    $s := IB;
    my $t = 7;
    $s := $t;
    $s = 8;
    is $t, 8, 'a later rebind to a variable clears the type-object mark';
}

{
    # An `is rw` accessor of an unset (type-object-holding) attribute is a
    # REAL Scalar container and must stay writable -- the allowlist must not
    # over-reach into this shape.
    class Foo { has $.x is rw; }
    my $f = Foo.new;
    my $r := $f.x;
    $r = 42;
    is $f.x, 42, 'an is-rw accessor of an unset attribute stays writable';
}

{
    # `return-rw` of a plain declared (never `:=`-bound) variable is a REAL
    # Scalar container too, holding its default Any type object.
    sub f() is rw { my $v; return-rw $v; }
    my $r := f();
    $r = 5;
    is $r, 5, 'return-rw of an Any-holding declared variable stays writable';
}
