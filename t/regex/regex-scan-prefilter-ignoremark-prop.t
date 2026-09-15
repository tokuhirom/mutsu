use Test;

# ADR-0099 Stage 1 (#8272): the unanchored-scan prefilter derives a
# first-character set for a `<:prop>` atom (by calling the engine's own
# property predicate over ASCII) and for a *scoped* `:ignoremark` sub-pattern
# (by walking the same mark-stripped tree the matcher walks). Both used to
# widen the derivation to "anything" or sink it outright.
#
# The prefilter can only ever be wrong by dropping a match it should have
# found, and every assertion here is a match it must still find. The Rust-side
# `tests/regex_prefilter_differential.rs` pins the same property against
# `MUTSU_REGEX_PREFILTER=off`; this file pins the behaviour a Raku program can
# actually observe.

plan 34;

# --- <:prop> -------------------------------------------------------------
is ('abcDef' ~~ / <:Lu> /).Str, 'D',        'an uppercase property is found';
nok 'abcdef' ~~ / <:Lu> /,                  'and is absent when it should be';
is ('ab12cd' ~~ m:g/ <:Nd> /).join(','), '1,2', 'every digit is found';
is ('abcDEFg' ~~ / <:Lu>+ /).Str, 'DEF',    'a quantified property is greedy';
is ('zz' ~~ / <:Lu> | 'zz' /).Str, 'zz',    'an alternation branch still matches';

# A property whose ASCII members are none at all: the derived set is empty over
# ASCII and admits every non-ASCII character, so the scan must still find one.
is ('abcΩdef' ~~ / <:Greek> /).Str, 'Ω',    'a non-ASCII-only property is found';
nok 'abcdef' ~~ / <:Greek> /,               'and fails on an ASCII subject';

# Negation inverts the predicate rather than widening the atom.
is ('12a34' ~~ / <:!Nd> /).Str, 'a',        'a negated property is found';
nok '12345' ~~ / <:!Nd> /,                  'and rejects an all-digit subject';

# The engine refuses a property at a position inside a grapheme cluster, and
# the prefilter must not turn that into a different answer.
my $with-mark = "a\x[094D]b";
nok $with-mark ~~ / <:Mn> /,                'a combining mark does not start a match';

is ('a b' ~~ m:g/ <:Zs> /).elems, 1,        'a space property is found';
is ('ab1' ~~ m:g/ <:Numeric_Value(1)> /).join(','), '1', 'a property with a value argument';
is ('abΩ' ~~ m:g/ <:sc<Latin>> /).join(','), 'a,b', 'a parameterized property';

# --- scoped :ignoremark ---------------------------------------------------
my $precomposed = "xcaf\x[e9]zz";
my $decomposed  = "xcafe\x[301]zz";

ok $precomposed ~~ / [:m 'cafe'] /,         'a precomposed subject matches';
ok $decomposed  ~~ / [:m 'cafe'] /,         'a decomposed subject matches';
is ($decomposed ~~ / [:m 'cafe'] /).from, 1, 'and starts at the right place';
nok 'zzz' ~~ / [:m 'cafe'] /,               'a miss is still a miss';

is ('qx' ~~ / 'q'? [:m 'x'] /).Str, 'qx',   'a scoped group after an optional atom';
is ("zz\x[e9]zz" ~~ / [:m <[a..e]>] /).Str, "\x[e9]", 'a class inside the group';
ok "zzCAF\x[c9]zz" ~~ / [:m :i 'cafe'] /,   ':i and :m together';
is ('abc' ~~ / [:m 'z'?] 'b' /).Str, 'b',   'a nullable group leaves the set open';

is ("caf\x[e9] cafe caf" ~~ m:g/ [:m 'cafe'] /).elems, 2, 'a :g scan finds both';
is "caf\x[e9]-cafe".subst(/ [:m 'cafe'] /, 'X', :g), 'X-X', 'a :g substitution';

# The positions mark stripping skips: a character that is itself stripped away,
# and one that does not start a grapheme cluster. The set is derived against
# the stripped subject, so it cannot speak for either -- both must be offered
# to the engine rather than rejected on the character sitting there.
# (The exact `.from` these report is a pre-existing mutsu/rakudo divergence and
# is deliberately not pinned here; `tests/regex_prefilter_engagement.rs` pins
# the prefilter half -- that such a position is offered to the engine at all.)
ok "\x[0300]x" ~~ / [:m 'x'] /,             'a match past a bare combining mark';
ok "\x[0600]x" ~~ / [:m 'x'] /,             'a match past a Prepend character';
# `\r\n` is one grapheme in Raku, so its `\n` half cannot start a match --
# in rakudo either. The prefilter must reproduce that, not "fix" it.
nok "a\r\nb" ~~ / [:m "\nb"] /,             'the \n half of a CRLF cluster does not match';
ok  "a\r\nb" ~~ / [:m 'b'] /,               'a match after a CRLF cluster';
nok "\x[0300]\x[0301]" ~~ / [:m 'a'] /,     'an all-marks subject still fails';

# Every non-ASCII character must be admitted: under `:ignoremark` the set's
# 'e' is a claim about a subject character that may be spelled any number of
# ways, and the reverse mapping is not enumerated.
ok "\x[1E17]" ~~ / [:m 'e'] /,              'a doubly-composed character matches';
ok "\x[0113]" ~~ / [:m 'e'] /,              'a macron-composed character matches';

# --- the two together, and against the plain forms ------------------------
is ("caf\x[e9]" ~~ / [:m 'cafe'] <:Lu>? /).Str, "caf\x[e9]", 'both in one pattern';
is ('CAFE' ~~ / <:Lu> [:m 'AFE'] /).Str, 'CAFE', 'a property leading a :m group';

# A top-level `:m` reaches the prefilter already stripped, so it must keep
# behaving exactly as it did.
ok "caf\x[e9]" ~~ / :m 'cafe' /,            'a top-level :m still matches';
is ("xcaf\x[e9]" ~~ / :m 'cafe' /).from, 1, 'and reports the right offset';
