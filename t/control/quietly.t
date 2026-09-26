use lib 'roast/packages/Test-Helpers/lib';
use Test;
use Test::Util;

plan 6;

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
