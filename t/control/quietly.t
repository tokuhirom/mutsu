use lib 'roast/packages/Test-Helpers/lib';
use Test;
use Test::Util;

plan 9;

dies-ok { quietly { die "not quiet enough" } }, '"die" in "quietly" dies';

is_run 'quietly { warn "muted" }; say "detum"',
    {
        status => 0,
        err => "",
        out => "detum\n",
    },
    '"warn" in "quietly" does not warn, does not die';

is_run 'quietly { say "loud" }',
    {
        status => 0,
        err => "",
        out => "loud\n",
    },
    '"say" in "quietly" works fine';

is_run 'quietly { note "eton" }; say "life"',
    {
        status => 0,
        err => "eton\n",
        out => "life\n",
    },
    '"note" in "quietly" works';

# GH-9607: rakudo's `quietly` installs its own CONTROL that resumes any
# CX::Warn, so a `warn` inside it never reaches a CONTROL handler installed
# outside the `quietly` block.
is_run 'CONTROL { when CX::Warn { say "W: ", .message; .resume } }; quietly { warn "q" }; say "end"',
    {
        status => 0,
        err => "",
        out => "end\n",
    },
    'an outer CONTROL never sees a warning suppressed by "quietly"';

# A CONTROL declared *inside* the quietly block is nested more tightly than
# quietly's own resume-everything handler, so it still gets first look.
is_run 'quietly { CONTROL { when CX::Warn { say "W: ", .message; .resume } }; warn "q" }; say "end"',
    {
        status => 0,
        err => "",
        out => "W: q\nend\n",
    },
    'a CONTROL declared inside "quietly" still handles its own warnings';

# GH-9656: an error escaping `quietly` must not leave warnings suppressed after
# it is caught -- the suppression is scoped to the quietly block alone.
is_run 'try { quietly { die 1 } }; warn "after"; say "still"',
    {
        status => 0,
        err => /after/,
        out => "still\n",
    },
    'a die escaping "quietly" does not leak its warning suppression';

is_run 'sub f { quietly { die 1 } }; { f(); CATCH { default { } } }; warn "after"',
    {
        status => 0,
        err => /after/,
    },
    'the same when the die is caught by a CATCH in an outer routine';

is_run 'quietly { try { quietly { die 1 } }; warn "hidden" }; warn "shown"',
    {
        status => 0,
        err => { $_ !~~ /hidden/ && $_ ~~ /shown/ },
    },
    'an enclosing "quietly" stays in force after an inner one is unwound';
