use v6.c;
use Test;

plan 13;

# PROCESS:: pseudo-package exposes the process-level dynamic variables
# (S02-names/pseudo-6c.t). $PROCESS::IN is the dynamic $*IN, in all three
# access forms: sigil-qualified, stash (.<>), and indirect ::("PROCESS").
{
    ok $PROCESS::IN === $*IN,          '$PROCESS::IN is $*IN';
    ok PROCESS::.<$IN> === $*IN,       'PROCESS::.<$IN> is $*IN';
    ok $::("PROCESS")::IN === $*IN,    '::("PROCESS") indirect is $*IN';
    ok $PROCESS::OUT === $*OUT,        '$PROCESS::OUT is $*OUT';
}

# PROCESS:: is writable: assigning through it updates the matching $* var,
# both for existing and brand-new process variables.
{
    $PROCESS::PROGRAM-NAME = "otter";
    is $*PROGRAM-NAME, "otter",        'existing $* assignable via PROCESS';
    $PROCESS::SOME_OTHER_VAR = "else";
    is $*SOME_OTHER_VAR, "else",        'new $* assignable via PROCESS';
}

# GLOBAL:: exposes top-level `our` declarations in every access form.
{
    { our $x60 = 60; }
    package A61 {
        is $GLOBAL::x60, 60,           '$GLOBAL::x60 works';
        is ::("GLOBAL")::('$x60'), 60, '::("GLOBAL") indirect works';
        is GLOBAL::.<$x60>, 60,        'GLOBAL::.<$x60> stash works';
    }
}

# The plain-lexical path is untouched: an ordinary variable named with a
# non-pseudo prefix still resolves normally, and `$*IN` itself is unchanged.
{
    my $x = 5;
    is $x, 5, 'ordinary lexical unaffected';
    ok $*IN.defined, '$*IN still resolves';
}

# @/% PROCESS forms map to the array/hash dynamic twigil keys.
{
    ok @PROCESS::ARGS eqv @*ARGS,      '@PROCESS::ARGS is @*ARGS';
    ok %PROCESS::ENV eqv %*ENV,        '%PROCESS::ENV is %*ENV';
}
