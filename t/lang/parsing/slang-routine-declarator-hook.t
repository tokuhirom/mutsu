use v6.e.PREVIEW;
use lib 't/lib';
use Test;

# A slang's new `routine-declarator:sym<...>` production is tried at the start of
# a statement (ADR-15762 slice 2, #13370).

plan 2;

use RoutineDeclaratorSlang;

fun greet { 'hello' }

is greet(), 'hello', 'the production declares a sub';
is greet(), 'hello', 'which can be called again';
