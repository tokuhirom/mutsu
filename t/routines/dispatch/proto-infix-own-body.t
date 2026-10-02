use Test;

plan 3;

# A proto whose body answers the call itself (no `{*}`) is an ordinary
# routine, also when spelled as an operator (mutsu#10696).
{
    proto sub infix:<foo>($a, $b) { 42 }
    is (1 foo 2), 42, 'proto infix with its own body is callable as an operator';
}

{
    proto sub infix:<bar>($a, $b) { $a + $b + 100 }
    is (1 bar 2), 103, 'proto infix body sees its arguments';
}

{
    proto sub infix:<qux>($a, $b) {*}
    multi sub infix:<qux>(Int $a, Int $b) { 9 }
    is (1 qux 2), 9, 'dispatching proto still reaches its candidate';
}
