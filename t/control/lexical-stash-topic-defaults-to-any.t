use v6;
use Test;

plan 5;

# `$_` is a declared lexical of every scope, `Any` until topicalized.
sub caller-topic { CALLER::LEXICAL::<$_> }
isa-ok caller-topic(), Any, 'CALLER::LEXICAL::<$_> is Any when nothing topicalized it';
ok caller-topic().WHAT === Any, 'and it is the Any type object, not Nil';

sub ord-of-topic(--> Int:D) { CALLER::LEXICAL::<$_>.ord }
dies-ok { ord-of-topic() }, 'a method call on the unset topic dies like Any.ord does';
dies-ok { Any.ord }, 'which is the same failure Any.ord gives';

sub unknown { CALLER::LEXICAL::<$no-such-var> }
ok unknown() ~~ Nil, 'an undeclared name still answers Nil';
