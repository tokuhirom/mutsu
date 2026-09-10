use v6;
use Test;

# A named parameter may carry aliases: `:s(:$sort)` answers to both `:s(…)` and
# `:sort(…)`. Binding already honoured both, but multi-candidate *matching* only
# looked at the first name, so a `multi` with an aliased named parameter rejected
# the long spelling with "No matching candidates" while the very same signature
# on a plain `sub` accepted it.
#
# Found via Prime::Factor, whose `multi divisors (Str $n, :s(:$sort) = False)`
# re-dispatches with `samewith (+$n).narrow, :sort($sort)` — that inner call had
# no matching candidate, so 3 of its 87 subtests never ran.

plan 12;

{
    multi f(Int $n, :s(:$sort) = False) { "int:$sort" }
    is f(1, :sort(True)), 'int:True', 'a multi accepts the long alias';
    is f(1, :s(True)),    'int:True', 'and the short name';
    is f(1),              'int:False', 'and the default still applies';
}

# A plain sub always worked — keep it pinned as the control.
{
    sub g(:s(:$sort) = False) { "g:$sort" }
    is g(:sort(True)), 'g:True', 'a plain sub accepts the long alias';
    is g(:s(True)),    'g:True', 'and the short name';
}

# Required (no default) must be satisfiable through either spelling.
{
    multi r(Int $n, :s(:$sort)!) { "r:$sort" }
    is r(1, :sort(True)), 'r:True', 'a required aliased named is satisfied by the long name';
    is r(1, :s(True)),    'r:True', 'and by the short name';
}

# The alias must not weaken candidate selection: a genuinely unknown named
# argument is still rejected.
{
    multi u(Int $n, :s(:$sort) = False) { 'u' }
    dies-ok { u(1, :nope(1)) }, 'an unknown named argument is still rejected';
}

# Type constraints, `where`, and methods take the same path.
{
    multi t(Int $n, Bool :s(:$sort) = False) { "t:$sort" }
    is t(1, :sort(True)), 't:True', 'a typed aliased named matches on the long name';

    multi w($n, :s(:$sort) where { True } = False) { "w:$sort" }
    is w(1, :sort(True)), 'w:True', 'a `where`-constrained aliased named too';

    my class C { multi method m(Int $n, :s(:$sort) = False) { "m:$sort" } }
    is C.new.m(1, :sort(True)), 'm:True', 'and a multi method';
}

# Candidate selection across two positional types still picks correctly with the
# aliased named present.
{
    multi p(Int $n, :s(:$sort) = False) { 'int' }
    multi p(Str $n, :s(:$sort) = False) { 'str' }
    is-deeply (p(1, :sort(True)), p('x', :sort(True))), ('int', 'str'),
        'the positional type still selects the candidate';
}
