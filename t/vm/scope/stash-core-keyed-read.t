use v6.*;
use Test;

# A one-key read of the `CORE::` stash with a run-time key is answered
# from the key alone, without building the whole core stash (#17698, #17776).
# These pin that the per-key answer matches what the whole stash reads.

plan 12;

sub say(*@) { 'user say' }

my $rev = 'v6c';
is CORE::{$rev}.WHO{'Int'}.^name, 'Int', 'CORE::{$rev}.WHO{"Int"} is the core type';
is CORE::{$rev}.WHO{'Str'}.^name, 'Str', 'CORE::{$rev}.WHO{"Str"} is the core type';
is CORE::<v6c>.WHO<Cool>.^name, 'Cool', 'literal key, then .WHO';
$rev = 'v6d';
is CORE::{$rev}.^name, 'CORE::v6d', 'CORE::{$rev} is the revision package';
ok CORE::{$rev}.WHO ~~ Associative, 'its .WHO is Associative';

my $key = '&say';
ok CORE::{$key} ~~ Callable, 'a built-in routine is answered by a run-time `&` key';
isnt CORE::{$key}(), 'user say', 'the user `sub say` does not shadow CORE::{"&say"}';
is CORE::{'&uc'}('abc'), 'ABC', 'CORE::{"&uc"} is the core routine';

my $missing = '&no-such-core-routine-xyz';
nok CORE::{$missing}.defined, 'a missing `&` key is not defined';
nok CORE::{'no-such-thing-xyz'}.defined, 'a missing plain key is not defined';
is SETTING::{'&uc'}('x'), 'X', 'SETTING:: answers the same way';

# The whole stash still lists the core routines.
ok CORE::.keys.grep('&uc'), 'CORE::.keys still lists &uc';
