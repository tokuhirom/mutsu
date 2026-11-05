use Test;

# A `try` that completes without dying leaves `$!` as `Nil` (rakudo's RakuAST
# frontend; the legacy frontend left the `Any` type object). So `$!.message`
# on the success path answers Nil instead of dying with "No such method".

plan 9;

try 1;
is $!.raku, 'Nil', 'after a successful try, $! is Nil';
is $!.message, Nil, '$!.message on the success path is Nil';
is $!.defined, False, '... and undefined';

try die "boom";
is $!.message, 'boom', 'after a failing try, $! is the exception';
try 2;
is $!.raku, 'Nil', 'a later successful try resets it';

{
    try 3;
    is $!.raku, 'Nil', 'in an inner block too';
}

my $r = try { 4 };
is $!.raku, 'Nil', 'a try block with a value';

for 1 {
    try { CONTROL { default { } }; next }
    is $!.raku, 'Nil', 'a handled control signal in try also resets it';
}

try { try { die "inner" }; is $!.message, 'inner', 'the inner failure is visible inside' }
