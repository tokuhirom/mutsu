use Test;

plan 5;

# `andthen`/`orelse` must NOT mis-invoke a plain instance RHS as `Foo()`
# (which failed with "No such method 'CALL-ME'"). The instance is the value.
{
    my class Meow { has $.a; }
    my $x;
    isa-ok ($x orelse Meow.new), Meow,
        'orelse instance RHS returns the instance, not a CALL-ME error';
}

# Same through a method call returning an instance, in a chain position.
{
    my class Box { has $.v; }
    my $x;
    isa-ok ($x orelse Box.new(:v(1)) andthen $_), Box,
        'orelse/andthen instance RHS is not mis-invoked';
}

# `.self` ignores arguments (fake-infix adverbs on `.= self`) and returns self.
{
    my Int $x;
    $x notandthen .=self :42moews :100foos notandthen .=new: 42;
    is-deeply $x, 42, '.=self ignores fake-infix adverbs; chain continues';
}

# A plain `.self` with a named arg returns the invocant.
{
    my $v = 5;
    is-deeply $v.self(:ignored), 5, '.self with a named arg returns self';
}

# Chained `.=new` writeback through orelse/andthen still works.
{
    my Int $x3;
    $x3 orelse .=new andthen .=new: 43;
    is-deeply $x3, 43, 'orelse/andthen chained .=new writeback';
}
