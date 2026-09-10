use Test;
use MONKEY-SEE-NO-EVAL;

# A placeholder parameter used directly in the MAINLINE -- outside any sub or
# block -- is `X::Placeholder::Mainline`. `$^x`, `@_` and `%_` are all
# placeholders, so `@_` at mainline is this error and NOT `X::Undeclared`, which
# is why the check has to run before the undeclared-variable check.
#
# EVAL compiles a fresh compilation unit, so an EVAL'd string is its own
# mainline and gets the same treatment.
#
# Verified assertion-for-assertion against rakudo.

plan 15;

sub class-of($code) { (try EVAL $code); $! ?? $!.^name !! 'no error' }

# --- placeholders at the EVAL'd unit's mainline ---------------------------
is class-of('$^a'),          'X::Placeholder::Mainline', 'a bare $^a at mainline';
is class-of('say $^a'),      'X::Placeholder::Mainline', '$^a inside a listop call';
is class-of('$^a + $^b'),    'X::Placeholder::Mainline', 'two placeholders';
is class-of('@_'),           'X::Placeholder::Mainline', '@_ is a placeholder, not undeclared';
is class-of('%_'),           'X::Placeholder::Mainline', '%_ likewise';
is class-of('"foo".{ say $^a }'), 'X::Placeholder::Mainline',
   'a postfix-block placeholder is still the mainline one';

# --- and it carries the placeholder's name --------------------------------
{
    try EVAL '$^x';
    is $!.placeholder, '$^x', 'the exception names the placeholder';
    ok $!.message.contains('outside of a sub or block'), 'and says why';
}
{
    try EVAL '@_';
    is $!.placeholder, '@_', 'the same for @_';
}

# --- a BLOCK is a different error -----------------------------------------
is class-of('do    { $^x }'), 'X::Placeholder::Block',
   'a placeholder in a `do` block is X::Placeholder::Block';
is class-of('class { $^x }'), 'X::Placeholder::Block',
   'and in a class body';

# --- where a placeholder is legal, nothing is raised ----------------------
is class-of('sub f { $^x }'),     'no error', 'a sub body may declare a placeholder';
is class-of('my $b = { $^x }'),   'no error', 'so may a block assigned to a variable';
is class-of('my $b = { @_ }'),    'no error', '@_ inside a block is fine too';

# --- an ordinary undeclared variable is still X::Undeclared ---------------
is class-of('$undeclared-thing'), 'X::Undeclared',
   'a plain undeclared variable is unaffected';

# vim: expandtab shiftwidth=4
