use v6;
use Test;

# A `~~` / single-match regex only needs the first (greedy, highest-priority)
# complete match. The backtracking DFS discovers that match first, so it now
# stops there instead of exploring the entire backtracking tree. This both
# preserves match/capture semantics and bounds catastrophic backtracking on
# ambiguous patterns (e.g. `(\w+) (\w+) (\w+) b` over a long run, which would
# otherwise explore exponentially many splits).

plan 9;

# --- ambiguous multi-group greedy split: leftmost-greedy wins ---
ok "aaaaaaaaaaaaaaaaaaaab" ~~ /(\w+) (\w+) (\w+) b/, 'ambiguous pattern matches';
is $0, 'aaaaaaaaaaaaaaaaaa', 'first group is greedy (16 of the a-run)';
is $1, 'a', 'second group gets one char';
is $2, 'a', 'third group gets one char';

# --- greedy vs frugal still behave ---
ok "abc123" ~~ /(\w+?)(\d+)/, 'frugal+greedy matches';
is ~$/, 'abc123', 'frugal first group yields the full overall match';

# --- anchored ambiguous split keeps greedy semantics ---
ok "aaa" ~~ /^(a+)(a+)$/, 'anchored ambiguous split matches';
is $0, 'aa', 'anchored greedy first group';

# --- a pattern that without early-exit would explode but still must match ---
ok "xxxxxxxxxxxxxxxxxxxxy" ~~ /(\w+) (\w+) (\w+) (\w+) y/,
    'four-group ambiguous pattern matches without hanging';
