use Test;

plan 4;

# throws-like runs its code in sink context (Raku sinks the result). A bare
# `gather {...}` as the final statement is therefore forced — its body runs and
# a `return` outside any routine surfaces as X::ControlFlow::Return.
# See roast/S32-exceptions/misc.t (old-issue-tracker for `gather { return }`).
throws-like 'gather { return 1 }', X::ControlFlow::Return,
    'bare gather with `return` as final statement throws X::ControlFlow::Return';

# A plain `return;` and a `return` inside a for-loop body likewise throw.
throws-like 'return;', X::ControlFlow::Return,
    'bare return at mainline throws X::ControlFlow::Return';
throws-like 'for ^5 { return; }', X::ControlFlow::Return,
    'return inside a for loop throws X::ControlFlow::Return';

# A gather with no escaping control flow must NOT be treated as throwing.
{
    lives-ok { my $s = gather { take 1; take 2 } },
        'gather without escaping control flow lives';
}
