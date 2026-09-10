use Test;

plan 6;

# A `for` loop with no block is X::Syntax::Missing (what => 'block').
throws-like 'for 1, 2', X::Syntax::Missing, what => 'block',
    'for loop without a block reports a missing block';
throws-like 'for 1, 2 -> $x', X::Syntax::Missing, what => 'block',
    'for loop with a pointy signature but no block reports a missing block';

# A valid for loop still parses and runs.
lives-ok { EVAL 'my @seen; for 1, 2 { @seen.push: $_ }; @seen' },
    'a for loop with a block still parses';

# A `constant` with no initializer is X::Syntax::Missing (what => 'initializer').
throws-like 'constant foo;', X::Syntax::Missing, what => /initializer/,
    'bare constant declaration reports a missing initializer';
throws-like 'constant ($a, $b) = 1, 2;', X::Syntax::Missing, what => /initializer/,
    'list-pattern constant reports a missing initializer';

# A valid constant still parses and runs.
lives-ok { EVAL 'constant foo = 42; foo' },
    'a constant with an initializer still parses';
