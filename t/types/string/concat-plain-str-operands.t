use Test;

# #19293: `~` on two plain Str operands takes a short path. It must still
# yield what the full path yields: NFC across the join, a shared left
# operand left untouched, and a user `infix:<~>` in scope winning.

plan 9;

is 'abc' ~ 'def', 'abcdef', 'two plain strings';
is ('e' ~ "\x[301]").chars, 1, 'a combining mark composes across the join';
is ('e' ~ "\x[301]").NFC.list, (0xE9,), 'and the result is NFC';

{
    my $a = 'left';
    my $b = $a;
    my $c = $a ~ '-right';
    is $a, 'left', 'the left operand is untouched';
    is $b, 'left', 'and so is an alias of it';
    is $c, 'left-right', 'the result';
}

{
    my $s = '';
    $s = $s ~ 'x' for ^1000;
    is $s.chars, 1000, 'an accumulation';
}

{
    multi sub infix:<~>(Str $a, Str $b where 'magic') { 'overridden' }
    is 'a' ~ 'magic', 'overridden', 'a user infix:<~> in scope wins';
    is 'a' ~ 'b', 'ab', 'and the built-in still answers the rest';
}
