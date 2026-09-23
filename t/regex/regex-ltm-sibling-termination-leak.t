use v6;
use Test;

plan 2;

# ADR-0022's `LTM_PREFIX_TERMINATED` thread-local records "this branch's own
# declarative-prefix walk hit a non-declarative stopper (a `<.ws>`, a code
# block, ...), so accept what has been measured so far and unwind." Both the
# singular (`regex_match_capture.rs`) and plural (`regex_match_atom.rs`)
# `Alternation` arms iterate every alternative of a `|` in one loop, calling
# straight into the real matcher (not through `ltm_prefix_len_at`, which
# saves/restores the flag around its own call) to check whether each
# alternative matches at all.
#
# A branch that hits a stopper leaves `LTM_PREFIX_TERMINATED` set to `true`
# after its own walk returns. Before the fix, nothing reset it before the
# NEXT alternative's own walk began, so a stopper hit while evaluating one
# branch corrupted every later sibling in the same loop: `walk_tokens`'s
# entry check fires whenever the flag is already set and accepts a bogus
# zero-width "match" before comparing a single atom of the sibling's own
# pattern — even when that sibling's real body cannot possibly match here.
#
# This is only observable when the `Alternation` runs while
# `LTM_DECLARATIVE_MODE` is already ambient (i.e. this whole alternation is
# itself being measured as a candidate of an ANCESTOR ranking decision) — a
# top-level `A | B` match never engages the flag. Proto-token dispatch is
# exactly such an ancestor: ranking candidate `thing:sym<a>` calls
# `ltm_prefix_len_at` on its whole body, which here contains a nested,
# unquantified `[ <.ws> | 'zz' ]` alternation — `<.ws>` (declared first) sets
# the flag, and the leak then makes the immediately-following `'zz'` branch
# look like a zero-width match too, undercounting `thing:sym<a>`'s true
# declarative prefix (3, via `'zz''X'`) down to 0. That made the shorter,
# single-token `thing:sym<b>` ('z', prefix 1) win the proto ranking, so the
# grammar reported a 1-character match instead of the real 3-character one.
grammar SiblingTerminationLeak {
    proto token thing {*}
    token thing:sym<a> { [ <.ws> | 'zz' ] 'X' }
    token thing:sym<b> { 'z' }
}

my $m = SiblingTerminationLeak.subparse('zzX', :rule<thing>);
ok $m.defined, 'proto dispatch matches';
is ~$m, 'zzX',
    "a stopper in an earlier sibling alternative does not truncate a later "
    ~ "sibling's own declarative-prefix measurement (raku picks the longer, "
    ~ "higher-ranked thing:sym<a> candidate)";
