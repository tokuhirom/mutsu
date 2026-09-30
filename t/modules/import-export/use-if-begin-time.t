use Test;

# ADR-0134 slice 3 (#9919): under the `if` pragma, the `:if(...)` value of a
# `use` is evaluated at BEGIN time,
# where a lexical holds only what a BEGIN stored. A run-time assignment has not
# run yet. All three pass under `RAKUDO_RAKUAST=1 raku -I modules/if/lib`, the
# frontend ADR-0098 matches; the legacy frontend's `if` path treats an
# undefined condition as false instead of rejecting it.

plan 3;

throws-like 'use if; my $c = True; use Totally::Missing::Runtime:if($c)', Exception,
    message => /'Did not provide compile-time-value for :if adverb in use statement'/,
    'a condition only a run-time assignment defines is undefined at BEGIN time';

eval-lives-ok 'use if; BEGIN my $c = False; use Totally::Missing::Begin:if($c)',
    'a condition a BEGIN declared is honoured';

eval-lives-ok 'use if; my $c; BEGIN $c = False; use Totally::Missing::Stored:if($c)',
    'a condition a BEGIN stored is honoured';
