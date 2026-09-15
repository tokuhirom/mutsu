use v6;
use Test;

# Regression (#7954, the `expected statement ...` parse-failure index): a BARE
# `+` is the anonymous single-argument-rule slurpy -- the `+@` spelling with the
# sigil left off, exactly as `*@` may be written without a name. `Data::Record`
# declares one in a proto:
#
#     proto MAKEOP(Str:D, +) {*}
#
# Only the sigilled (`+@a`) and sigilless-NAME (`+foo`) forms were parameters at
# all, so a signature holding a bare `+` failed at its closing paren and took the
# whole enclosing file down with it.

plan 9;

sub alone(+) { 'ok' }
is alone(1, 2), 'ok', 'a bare + is a parameter on its own';
is alone(), 'ok', '... and binds no arguments happily';

sub after-positional($a, +) { $a }
is after-positional(1, 2, 3), 1, 'a bare + after a positional';

sub before-return(+ --> Str) { 'ret' }
is before-return(1), 'ret', 'a bare + before a --> return constraint';

# The proto/multi shape the distribution writes.
proto MAKEOP(Str:D, +) {*}
multi MAKEOP('a', +types) { types.raku }
is MAKEOP('a', 1, 2), '(1, 2)', 'a bare + in a proto still dispatches to a named multi';

# The spellings that already worked are unchanged.
sub sigilled(+@a) { @a.raku }
is sigilled(1, 2), '[1, 2]', '+@a is unchanged';
sub anon-sigilled(+@) { 'ok' }
is anon-sigilled(1, 2), 'ok', '+@ is unchanged';
sub named-sigilless(+x) { x.raku }
is named-sigilless(1, 2), '(1, 2)', '+foo is unchanged';

# Infix `+` in an ordinary expression is untouched.
my $a = 1;
my $b = 2;
is $a + $b, 3, 'infix + is still addition';
