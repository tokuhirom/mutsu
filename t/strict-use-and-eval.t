use Test;
plan 3;

no strict;

class Foo {
    $foo = 42;
}

is $Foo::foo, 42, 'lax-declared variable in class is package scoped';

{
    use strict;
    throws-like '$foo = 10;', X::Undeclared, suggestions => 'Foo';
}

lives-ok { EVAL '(6;)' }, 'EVAL accepts parenthesized statement list';
