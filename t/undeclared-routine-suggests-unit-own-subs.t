use Test;

# Rakudo's CHECK-time "Undeclared routine" error suggests a close name, and the
# candidates include the compilation unit's OWN subs, not only core routines:
#
#   sub greeting() { 1 }; greetng()
#     -> Undeclared routine:
#            greetng used at line 1. Did you mean 'greeting'?
#
# mutsu drew its candidates from the interpreter's registered routines, which do
# not hold the unit's own declarations at the point the check runs, so it
# reported the typo with no way to see what was meant. The CHECK-time walker had
# already collected those names; it now passes them along as suggestion
# candidates.
#
# The distinction that matters: the walker collects every declared name
# scope-blind -- variables and types included -- because suppressing a call on
# any of them is the safe direction. Suggestions must come from the *routine*
# subset only, or a `my $greeting` would be offered as the routine you meant,
# which rakudo never does.

plan 4;

throws-like 'sub greeting() { 1 }; greetng()', X::Undeclared::Symbols,
    "a typo'd call suggests the unit's own sub",
    message => /"Did you mean 'greeting'"/;

throws-like 'multi sub handle(Int) { 1 }; handel(1)', X::Undeclared::Symbols,
    'a multi candidate is a suggestion candidate too',
    message => /"Did you mean 'handle'"/;

# Core routines were already suggested and must stay so.
throws-like 'elem([1, 2])', X::Undeclared::Symbols,
    'a core routine is still suggested',
    message => /"Did you mean 'elems'"/;

# A variable of the near-miss name must NOT be offered as a routine.
my $message = '';
try {
    EVAL 'my $greeting = 1; greetng()';
    CATCH { default { $message = .message } }
}
nok $message.contains('Did you mean'),
    'a same-named variable is not offered as the routine you meant';
