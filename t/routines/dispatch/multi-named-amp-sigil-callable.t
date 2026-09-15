use Test;

# The `&` sigil on a NAMED parameter is a Callable constraint, exactly as it
# is on a positional one. mutsu enforced it for positionals only, so a
# `:&handler!` candidate happily claimed a plain string and out-dispatched the
# sibling `Str:D :$handler!` candidate. JSON::Marshal declares that very pair
# for `is marshalled-by`, so `is marshalled-by('Str')` ran the Callable
# candidate and died mixing a Str into a `has &.marshaller` slot (#8121).
#
# Measured against raku v2026.07; this file passes verbatim there too.

plan 7;

multi sub tm-pick(:&handler!) { 'code' }
multi sub tm-pick(Str:D :$handler!) { 'str' }

is tm-pick(handler => 'Str'), 'str', 'a Str named arg picks the Str candidate';
is tm-pick(handler => { 42 }), 'code', 'a Block named arg picks the & candidate';
is tm-pick(handler => &tm-pick), 'code', 'a Sub named arg picks the & candidate';

# A lone `&`-sigil candidate simply does not match a non-Callable, rather than
# matching and then failing to bind.
multi sub only-code(:&cb!) { 'code' }
multi sub only-code(:$cb!) { 'any' }

is only-code(cb => 3), 'any', 'a non-Callable falls through to the untyped candidate';
is only-code(cb => -> { 1 }), 'code', 'a Callable still reaches the & candidate';

# The aliased spelling carries the same constraint.
multi sub aliased(:c(:&cb)!) { 'code' }
multi sub aliased(:c(:$cb)!) { 'any' }

is aliased(c => 'x'), 'any', 'an aliased & named param rejects a non-Callable';
is aliased(c => { 1 }), 'code', 'and accepts a Callable';
