use v6;
use Test;

# Regression: `:=` binding is an expression-level operator (item-assignment
# precedence) in raku, so it must work unparenthesized after `return`, in a
# declarator RHS, etc. mutsu previously only handled `:=` at the statement level,
# so `return $x := 5` failed to parse ("expected statement").
# Surfaced loading Zef::Distribution:
#   return @!provides-specs := @provides-specs;

plan 8;

# `return EXPR := EXPR`
{
    my $x;
    sub f { return $x := 5 }
    is f(), 5, 'return $x := 5 returns the bound value';
    is $x, 5, 'the binding took effect';
}

# `return @arr := @other` (array binding)
{
    my @x;
    my @y = 1, 2, 3;
    sub g { return @x := @y }
    is g().elems, 3, 'return @x := @y binds and returns the array';
}

# Binding inside a declarator RHS.
{
    my $x;
    my $y = $x := 5;
    is $x, 5, 'declarator RHS: inner bind took effect';
    is $y, 5, 'declarator RHS: outer var got the bound value';
}

# Hash binding via return.
{
    my %h;
    my %src = a => 1, b => 2;
    sub h { return %h := %src }
    is h()<a>, 1, 'return %h := %src binds the hash';
}

# The bare statement form still works (was already supported).
{
    my @x;
    @x := [10, 20];
    is @x[1], 20, 'bare-statement := binding still works';
}

# Compile-time `::=` is NOT mis-consumed by the new `:=` path.
{
    constant pi-ish ::= 3;
    is pi-ish, 3, '::= compile-time bind unaffected';
}
