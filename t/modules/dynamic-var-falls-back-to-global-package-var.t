use v6;
use Test;

plan 8;

# A dynamic variable nobody declared in the dynamic scope falls back to the
# same-named package variable of GLOBAL (`%*X` reads `%GLOBAL::X`).
sub scalar-read { $*SCALAR-FALLBACK }
sub array-read { @*ARRAY-FALLBACK }
sub hash-read { %*HASH-FALLBACK }

$GLOBAL::SCALAR-FALLBACK = 7;
@GLOBAL::ARRAY-FALLBACK = 1, 2, 3;
%GLOBAL::HASH-FALLBACK = a => 5;

is scalar-read(), 7, '$*X reads $GLOBAL::X';
is-deeply array-read().List, (1, 2, 3), '@*X reads @GLOBAL::X';
is hash-read()<a>, 5, '%*X reads %GLOBAL::X';

sub defined-and-exists {
    %*DEFAULTS.defined ?? (%*DEFAULTS<default>:exists ?? 'has default' !! 'no default') !! 'undef'
}
is defined-and-exists(), 'undef', 'an unset hash stays undefined';
%GLOBAL::DEFAULTS = default => 42;
is defined-and-exists(), 'has default', 'the idiom Red uses for its default drivers';
is %*DEFAULTS<default>, 42, 'and reads straight from the mainline';

# A real dynamic declaration still wins.
{
    my %*DEFAULTS = default => 'dynamic';
    is %*DEFAULTS<default>, 'dynamic', 'a declared dynamic variable wins over GLOBAL';
}
is %*DEFAULTS<default>, 42, 'and the GLOBAL value is back after the scope';
