use Test;

plan 4;

# `constant term:<NAME> = value` defines a term named NAME, resolvable as a
# bareword — exactly as `constant NAME = value` would if NAME were a legal
# identifier. Previously mutsu parsed only the `term` head and failed to find
# the initializer ("Missing initializer on constant declaration").

constant term:<X> = 'heart';
is X, 'heart', 'constant term:<X> defines a bareword term';

constant term:<♥> = '♥';
is ♥, '♥', 'constant term:<...> works with a non-ASCII symbol';

# Guillemet delimiters are equivalent to angle brackets.
constant term:«Y» = 42;
is Y, 42, 'constant term:«Y» (guillemets) defines a term';

# The value keeps its type.
constant term:<PI> = 3.14;
is PI.WHAT.^name, 'Rat', 'constant term keeps the value type';
