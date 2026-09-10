use Test;

plan 14;

# Helper: render .caps as 'key:value|...'
sub ca(@x) { join '|', @x.map({ .key ~ ':' ~ .value }) }

# --- Conjunction (&&, &) captures: all branches' captures are kept, and all
#     branches must cover the same substring. ---

ok 'a' ~~ m/<alpha> && a/, 'conjunction matches (alpha && a)';
is ca($/.caps), 'alpha:a', 'conjunction keeps first-branch named capture';

ok 'a' ~~ m/a && <alpha>/, 'conjunction matches (a && alpha)';
is ca($/.caps), 'alpha:a', 'conjunction keeps last-branch named capture';

ok 'a' ~~ m/<alpha> & <ident>/, 'conjunction matches (alpha & ident)';
is ca($/.caps.sort(*.key)), 'alpha:a|ident:a', 'conjunction merges all branch captures';

# A conjunction with differing match lengths fails (same-substring rule).
nok 'abc' ~~ m/\w\w && <alpha>/, 'conjunction fails when branches differ in length';

# Quantified conjunction inside a capture group: group captures, not leaked names.
ok 'ab' ~~ m/([a|b] && <alpha>)**1..2/, 'quantified conjunction in group matches';
is ca($/.caps), '0:a|0:b', 'quantified group conjunction yields positional captures';

# --- Named captures inside a positional group are sub-captures, not top-level. ---
ok 'a' ~~ m/(<alpha>)/, 'named capture inside positional group matches';
is ca($/.caps), '0:a', 'named-in-group is a positional capture, not top-level named';

# --- Separator quantifiers (% / %%) fold each side into its own capture group. ---
ok 'a;b,c,' ~~ m/(<.alpha>) +% (<.punct>)/, '% separator with captures matches';
is ca($/.caps), '0:a|1:;|0:b|1:,|0:c', '% separator folds atom/sep into separate groups';

ok 'a;b,c,' ~~ m/(<.alpha>) +%% (<.punct>)/, '%% separator with captures matches';
