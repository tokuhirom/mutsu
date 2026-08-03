use v6;
use Test;

plan 11;

# Three constructs rakudo diagnoses as X::Syntax::Malformed. mutsu had already
# rejected all three, but each rejection was a *soft* parse error, so the
# declaration/call alternative backtracked and the failure came out as the
# parser's generic "Confused." with no class at all.

# `my $x = ` — the `=` is the commit point, so an unreadable RHS is a malformed
# initializer rather than a reason to reconsider the whole declaration.
throws-like 'my $x = ', X::Syntax::Malformed, what => 'initializer';
throws-like 'my @a = ', X::Syntax::Malformed, what => 'initializer';
throws-like 'my $x = ;', X::Syntax::Malformed, what => 'initializer';

# ...but only when *nothing* of the RHS could be read. An initializer that
# parsed part of itself has already produced the better diagnosis, and that one
# wins — flattening every failure to "Malformed initializer" masked these two.
throws-like 'my @a = 1, => 2', X::Syntax::InfixInTermPosition, infix => '=>';
throws-like 'my $foo = { given 1 { when Real { 1 } when Str { 2 } } };',
    X::Syntax::Confused;

# `.::` — a class-qualified postfix call with no name after the `::`. The
# postfix form already said so; the *topic* form (`.::` at statement start)
# never reached that check.
throws-like '.::', X::Syntax::Malformed, what => 'class-qualified postfix call';
throws-like 'my $x; $x.::', X::Syntax::Malformed,
    what => 'class-qualified postfix call';

# `:7` — digits after the colon with no `<`/`(`/`[` body and no identifier is a
# malformed radix number. A combining mark is not alphabetic, so the synthetic
# numeral `:7\x[308]a` lands here too rather than reading as a `:name(7)` pair.
throws-like ':7', X::Syntax::Malformed, what => 'radix number';
throws-like ":7\x[308]a", X::Syntax::Malformed, what => 'radix number';

# The neighbouring forms these three must not swallow.
is-deeply EVAL(':7a'), (a => 7), ':<digits><identifier> is still a numeric colonpair';
is :16<ff>, 255, 'a radix literal with a body still parses';
