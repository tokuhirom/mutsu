use Test;

# A `X ** min..max` (no separator) bounded-range quantifier is lowered by
# `expand_ltm_pattern`'s string-based LTM expansion, guarded by
# `is_single_regex_atom`: only a *single* atom immediately before `**` may be
# repeated this way, otherwise the whole preceding sequence would be spliced
# into a string and re-parsed, corrupting it.
#
# `is_single_regex_atom` used to check only the first and last character of
# the candidate atom text (`'` ... `'`, `[` ... `]`, etc). The count-spec
# regex that finds `**min..max` is `(.+?)\*\*(...)$`, which is not anchored to
# the atom immediately before the LAST `**` — when an EARLIER token in the
# same pattern also has its own `**N` quantifier, the greedy match captures
# the *entire* multi-token prefix (up to and past that earlier `**`) as
# "the atom". If that whole prefix happens to start and end with a quote
# character (because its first and last tokens are quoted literals), the old
# first/last-char check misclassified the multi-token prefix as a single bare
# string literal and string-expanded it — repeating the *whole prefix text*
# (garbling the earlier `**N` quantifier and swallowing any atoms after it)
# instead of leaving the sequence to the normal per-token parser (issue
# #8453).
plan 8;

# The exact issue #8453 repro: an optional leading atom, a counted `\d ** 2`,
# two more atoms, then a literal `'b' ** 1..3` range at the end.
ok "1234b" ~~ / 'x'? \d ** 2 . . 'b' ** 1..3 /, 'issue #8453 repro matches (subject has b)';
ok "99zzbb" ~~ / 'x'? \d ** 2 . . 'b' ** 1..3 /, 'issue #8453 repro matches (subject has bb)';
nok "12abcb" ~~ / 'x'? \d ** 2 . . 'b' ** 1..3 /, 'issue #8453 repro does not match (wrong shape)';
nok "c-0" ~~ / 'x'? \d ** 2 . . 'b' ** 1..3 /, 'issue #8453 repro does not match (too short)';

# Narrowing table from the issue: each of these must NOT throw
# "Quantifier range is empty" against "c-0".
nok "c-0" ~~ / 'x'? \W ** 2 . . 'b' ** 1..3 /, 'character class instead of \d still does not throw';
nok "c-0" ~~ / 'x'? \W ** 2 . . 'b' ** 1..1 /, 'a **1..1 range on the trailing literal still does not throw';

# The earlier quantifier's own range must survive intact: exactly 2 digits
# are required, not fewer (which the corrupted `Some(0)` max would have
# permitted).
ok "12bb" ~~ / \d ** 2 . 'b' ** 1..3 /, 'earlier \d ** 2 still requires exactly two digits (matches)';
nok "1bb" ~~ / \d ** 2 . 'b' ** 1..3 /, 'earlier \d ** 2 still requires exactly two digits (one digit fails)';
