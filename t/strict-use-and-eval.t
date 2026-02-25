use Test;
plan 5;

no strict;

class Foo {
    $foo = 42;
}

is $Foo::foo, 42, 'lax-declared variable in class is package scoped';

{
    use strict;
    throws-like '$foo = 10;', X::Undeclared, suggestions => 'Foo';
}

{
    use strict;
    {
        no strict;
        $tmp = 7;
        is $tmp, 7, '`no strict` disables strict mode in nested lexical scope';
    }
    throws-like '$tmp2 = 8;', X::Undeclared, '`strict` is restored after nested `no strict` block';
}

lives-ok { EVAL '(6;)' }, 'EVAL accepts parenthesized statement list';
