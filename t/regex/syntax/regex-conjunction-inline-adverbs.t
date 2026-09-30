use Test;

# Each branch of a top-level `&` / `&&` is parsed as a pattern of its own, so an
# inline adverb the enclosing regex consumed before the split has to be put
# back on every branch, exactly as it already is for `|` / `||` (#10353).
# `:r` (ratchet) made `\w+` possessive in an alternation branch but not in a
# conjunction one, and `:s` (sigspace) was dropped the same way. Expectations
# are rakudo's.

plan 23;

# --- :r reaches every branch of a conjunction ---
is ('xyz' ~~ / :r \w+ & xy /).gist, 'Nil', ':r makes \w+ possessive in the first branch';
is ('xyz' ~~ / :r xy & \w+ /).gist, 'Nil', ':r makes \w+ possessive in the second branch';
is ('xyz' ~~ / :r \w+ && xy /).gist, 'Nil', ':r reaches the branches of &&';
is ('xyz' ~~ / :r [ \w+ & xy ] /).gist, 'Nil', ':r reaches a grouped conjunction';
is ('xyz' ~~ / :ratchet \w+ & xy /).gist, 'Nil', 'the spelled-out :ratchet does the same';

# --- the same patterns without :r still backtrack ---
is ('xyz' ~~ / \w+ & xy /).gist, '｢xy｣', 'without :r the first branch backtracks to xy';
is ('xyz' ~~ / \w+ && xy /).gist, '｢xy｣', 'without :r the same holds for &&';

# --- a ratcheted conjunction can still match ---
is ('xyz' ~~ / :r \w+ & xyz /).gist, '｢xyz｣', 'both branches take the same span';
is ('xyz' ~~ / :r x & x /).gist, '｢x｣', 'a single-token conjunction matches';

# --- :r scoped to a group does not leak outside it ---
is ('xyz' ~~ / [ :r \w+ ] & xy /).gist, 'Nil', ':r inside a group makes that branch possessive';
is ('xyz' ~~ / \w+ & [ :r xy ] /).gist, '｢xy｣', ':r inside the second branch leaves the first free to backtrack';

# --- the alternation behaviour this mirrors is unchanged ---
is ('xyz' ~~ / :r [\w+] z /).gist, 'Nil', 'a ratcheted group is possessive';
is ('xyz' ~~ / :r [\w+ | q] z /).gist, 'Nil', 'a ratcheted alternation is possessive';

# --- a named rule and a grammar token ---
my regex ratcheted-conj { :r \w+ & xy }
is ('xyz' ~~ /<ratcheted-conj>/).gist, 'Nil', 'a regex declared with :r is possessive in a conjunction';
grammar G { token TOP { \w+ & xy } }
is G.parse('xyz').gist, 'Nil', 'a token is ratcheted, so a conjunction in it is possessive';
is G.parse('xy').gist, '｢xy｣', '... and it still matches when both branches take the same span';

# --- :s reaches every branch of a conjunction ---
is ('a b' ~~ / :s a b & a b /).gist, '｢a b｣', ':s puts the whitespace matcher on both branches';
is ('ab' ~~ / :s a b & a b /).gist, 'Nil', '... so the branches no longer match without whitespace';
is ('a b' ~~ / :s a b & ab /).gist, 'Nil', 'a branch with no whitespace does not match the spaced text';
is ('a b' ~~ / :s a b && a b /).gist, '｢a b｣', ':s reaches the branches of && too';

# --- :i still reaches every branch ---
is ('XY' ~~ / :i xy & xy /).gist, '｢XY｣', ':i reaches both branches';
is ('XY' ~~ / :i xy & XY /).gist, '｢XY｣', ':i applies to a branch already in upper case';
is ('xy' ~~ / :i [ x | y ] & x /).gist, '｢x｣', ':i reaches a branch that is itself an alternation';
