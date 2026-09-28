use Test;

# `&foo.candidates».multi` (#9818): each candidate `routine_candidate_subs`
# builds for a `multi sub` handle must itself answer `.multi` True, even when
# the multi has only one candidate declared so far -- `multi`-ness is a fact
# about how the candidate was DECLARED (its registry key is arity-qualified,
# `registration_sub.rs`'s `multi_prefix`), not about how many candidates
# happen to survive `.candidates`' dedup. A plain (non-multi) sub's own
# `.candidates` entry must stay False. Ground truth gathered against `raku`.

sub plain { 1 }
my @plain-candidates = &plain.candidates;
is @plain-candidates.elems, 1, 'a plain sub has exactly one candidate';
nok @plain-candidates[0].multi, "a plain sub's own candidate: multi is False";

multi solo (Int $x) { $x }
my @solo-candidates = &solo.candidates;
is @solo-candidates.elems, 1,
        'a multi sub with only one candidate declared has one candidate';
ok @solo-candidates[0].multi,
        'a lone multi candidate: multi is True even with no sibling yet';

multi mm (Int $x) { "int" }
multi mm (Str $x) { "str" }
my @mm-candidates = &mm.candidates;
is @mm-candidates.elems, 2, 'a two-candidate multi exposes both candidates';
for @mm-candidates -> $c {
    ok $c.multi, 'each candidate of a multi family: multi is True';
}

done-testing;
