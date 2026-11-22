use Test;

# A built-in operator's code value taken while user candidates of it exist
# (`&[cmp]` beside `multi infix:<cmp>(Foo, Foo)`) still answers a call none of
# them accepts with the operator's own body, even from a scope that binds a
# lexical `&infix:<cmp>` of its own (#17631).

plan 8;

sub call(&c, $a, $b) { c($a, $b) }
my &plain = &[cmp];
{
    my &infix:<cmp> = -> $a, $b { "lexical" };
    is call(&plain, 1, 2), Less, 'the core candidate answers Ints beside a caller lexical';
    is plain(2, 1), More, '... also when the value is called directly';
}

class Foo { }
multi sub infix:<cmp>(Foo $a, Foo $b) { "foo" }
multi sub infix:<+>(Foo $a, Foo $b) { "plus" }

is call(&plain, Foo, Foo), 'foo', 'the user candidate answers its own types';

my &plus = &[+];
is plus(1, 2), 3, 'the core body of another operator';
is plus(Foo, Foo), 'plus', '... and its user candidate';
{
    my &infix:<+> = -> $a, $b { "lexical" };
    is plus(2, 3), 5, 'the value is the operator, not the caller lexical';
}
is plain(1 | 3, 2), any(Less, More), 'a Junction operand autothreads';
is-deeply (3, 1, 2).sort(&plain).List, (1, 2, 3), 'the value as a sort comparator';
