use Test;

# A range quantifier `** N..M` on a non-capturing atom must NOT introduce a
# spurious positional capture. Previously mutsu's LTM string-expansion wrapped
# the length alternatives in a capturing group `(aaa|aa)`, leaking `$/[0]`.

plan 14;

# --- No capture from a plain range-quantified atom ---
ok "aaa" ~~ /a ** 2..3/, 'a ** 2..3 matches';
is $/.Str, 'aaa', 'matches greedily up to the max';
is $/.elems, 0, 'no positional captures leak';
is $/.list.elems, 0, '.list is empty';

ok "aaaa" ~~ /a ** 2..3/, 'matches at most max repetitions';
is $/.Str, 'aaa', 'stops at the max of the range';

ok "a" ~~ /a ** 0..3/, '0..3 matches a single rep';
is $/.elems, 0, '0..M form also has no captures';

# --- A bracketed (non-capturing) atom likewise captures nothing ---
ok "abcabc" ~~ /[abc] ** 1..2/, 'non-capturing bracket repeats';
is $/.elems, 0, 'bracket-group range quantifier has no captures';

# --- An explicit capture group still captures every repetition ---
ok "aaaa" ~~ /(a) ** 2..3/, 'capturing group repeats';
is $/[0].elems, 3, 'a captured atom keeps all its matches';

# --- Fixed count (** N) was already correct; keep it that way ---
ok "aaa" ~~ /a ** 2/, 'fixed count matches';
is $/.elems, 0, 'fixed count has no captures';
