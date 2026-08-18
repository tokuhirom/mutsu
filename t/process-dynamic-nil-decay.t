use v6.c;
use Test;

plan 4;

# `PROCESS::<$x> = Nil` must decay to the `Any` type object, matching an
# ordinary untyped scalar assignment (`$x = Nil` leaves `$x === Any`), not
# store a literal Nil. See
# todo/tickets/process-dynamic-write-nil-not-decayed-to-any.md.
{
    PROCESS::<$SOME-DYNAMIC-VAR> = 42;
    PROCESS::<$SOME-DYNAMIC-VAR> = Nil;
    is PROCESS::<$SOME-DYNAMIC-VAR>.^name, 'Any',
        'PROCESS::<$x> = Nil decays to Any (via .^name)';
    ok PROCESS::<$SOME-DYNAMIC-VAR> === Any,
        'PROCESS::<$x> = Nil decays to Any (via ===)';
}

# The runtime-key form (`PROCESS::{$k} = Nil`, the same op a `//=`/`||=`
# compound assignment desugars into) goes through the same store path.
{
    my $key = '$OTHER-DYNAMIC-VAR';
    PROCESS::{$key} = 42;
    PROCESS::{$key} = Nil;
    is PROCESS::{$key}.^name, 'Any',
        'PROCESS::{$k} = Nil decays to Any (via .^name)';
    ok PROCESS::{$key} === Any,
        'PROCESS::{$k} = Nil decays to Any (via ===)';
}
