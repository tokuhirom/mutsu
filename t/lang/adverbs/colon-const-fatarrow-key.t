use v6;
use Test;

plan 6;

# A leading-`::` pseudo-package form on the LHS of `=>` is a symbol lookup that
# is evaluated to its value, NOT autoquoted like a plain bareword. So
# `::V => 42` keys on V's value ('x'), while `V => 42` keys on the string "V".

my constant V = "x";

{
    my %h = V => "oi", ::V => 42;
    is %h.raku, '{:V("oi"), :x(42)}',
        '::V => uses the constant value as key; V => autoquotes';
}

{
    # ::V alone (positional pair) evaluates the constant value.
    my %h = ::V => 42;
    is %h.raku, '{:x(42)}', 'bare ::V => value keys on the constant value';
}

{
    # A numeric constant value stringifies as the hash key.
    my constant K = 7;
    my %h = ::K => "v";
    is %h.raku, '{"7" => "v"}', 'numeric constant value becomes the key';
}

# Plain bareword keys are still autoquoted (named-argument semantics).
{
    my %h = a => 1, b => 2;
    is %h.raku, '{:a(1), :b(2)}', 'plain bareword keys autoquote';
}

# Qualified names (Bool::True) still evaluate to their value.
{
    my %h = Bool::True => 5;
    is %h.raku, '{:True(5)}', 'qualified name key evaluates to its value';
}

# `::V => x` produces a *positional* pair (value key), so it lands in the
# slurpy positional list, not the slurpy named hash.
{
    sub f(*@a, *%h) { "@a.raku()|%h.raku()" }
    is f(::V => 42), '[:x(42)]|{}',
        '::V => makes a positional pair, not a named argument';
}
