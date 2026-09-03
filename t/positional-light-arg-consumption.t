use Test;

# The positional-light call path binds its parameters straight out of the VM
# stack instead of copying them into an intermediate argument buffer. Pin the
# two invariants that design rests on:
#
#   1. Every exit path consumes the arguments, so the caller's expression
#      stack is left exactly as it was before the call. A leaked argument
#      slot would surface as a wrong value for the surrounding expression.
#   2. A parameter type-check failure still reports the COMPLETE, unmodified
#      argument list -- the checks run before any argument is moved out of
#      its slot, so a failure on the second parameter still sees the first.
#
# (mutsu raises X::TypeCheck::Argument here where Rakudo raises
# X::TypeCheck::Binding::Parameter; that divergence is pre-existing and is not
# what this file is about -- it only asserts the argument list stays whole.)

plan 9;

sub two(Int $a, Int $b --> Int) { $a * 10 + $b }

# 1. Arguments consumed on the success path, inside a larger expression.
is two(1, 2), 12, 'positional-light call returns the bound result';
is 100 + two(3, 4) + 1000, 1134, 'call inside an expression leaves the stack balanced';
is two(two(1, 2), two(3, 4)), 154, 'nested calls each consume their own arguments';

# 2. Type-check failure reports the full argument list, failing param second.
#    The bad value comes from a variable so this is a genuine run-time binding
#    failure rather than something a compile-time signature check could reject.
my $bad = "x";
my $err;
try {
    two(1, $bad);
    CATCH { default { $err = $_ } }
}
ok $err.defined, 'a failing type check throws';
is $err.message, 'Type check failed in binding $b: expected Int, got Str',
    'the message names the failing parameter';
is-deeply $err.arguments.List, ('Int', 'Str'),
    'the argument list still holds the already-bound first argument';

# 3. The stack is unwound on the error path too: a later call in the same
#    scope still sees a balanced stack.
is two(5, 6), 56, 'a call after a failed one still binds correctly';

# 4. Arity errors consume the arguments as well.
my $one = 7;
my $few;
try {
    two($one);
    CATCH { default { $few = $_.message } }
}
like $few, /'Too few positionals'/, 'a shortfall is an arity error';
is two(8, 9), 89, 'a call after an arity error still binds correctly';
