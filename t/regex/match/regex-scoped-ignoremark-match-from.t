use Test;

# Issue #8485: a scoped `:ignoremark` sub-pattern entered at a position whose
# character stripping removes (a bare combining mark) must report the match
# as starting at the first character it actually consumed, not at the
# stripped-away position it was offered.
#
# `regex_match_ends_from_caps_in_pkg_impl` (src/runtime/regex/regex_match_core.rs)
# enters the group at `derived_start = original_to_stripped(start)`, which
# already skips the stripped character to reach the first surviving one, but
# used to report the match as starting at the original `start` anyway.
#
# `t/regex/regex-scan-prefilter-ignoremark-prop.t` deliberately does not pin
# `.from` for this family; this file does.

plan 6;

# The bug's own repro: a bare combining mark that the scan offers as a start
# candidate, stripped entirely before the sub-pattern gets to try anything.
my $m = ("\x[0300]x" ~~ / [:m "x"] /);
is $m.from, 1, 'a scoped :m match skips a leading stripped-away combining mark';
is $m.to, 2, '...and .to is unaffected';
is $m.Str, "x", '...and the matched text does not include the stripped mark';

# When nothing precedes the stripped position, `start` and the corrected
# start coincide -- must not regress the existing (already-pinned) case.
my $decomposed = "xcafe\x[301]zz";
is ($decomposed ~~ / [:m 'cafe'] /).from, 1, 'the already-pinned decomposed-subject case is unaffected';

# A scoped :m group is not always the first atom in the pattern: a preceding
# atom's own match must not be dropped by the group's internal start
# correction once merged into the overall captures.
is ('qx' ~~ / 'q'? [:m 'x'] /).Str, 'qx',
    'a preceding atom is not lost when the scoped group needs no correction';
is ('CAFE' ~~ / <:Lu> [:m 'AFE'] /).Str, 'CAFE',
    'same, with a property atom leading the scoped group';
