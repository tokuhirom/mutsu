use Test;

# ADR-0099 §5 (#8272, final Stage 1 slice): the unanchored-scan prefilter's
# NFA over the declarative leading run of a pattern -- narrowing a scan using
# more than just its very FIRST character, the way every mechanism before it
# was limited to.
#
# The prefilter can only ever be wrong by dropping a match it should have
# found, and every assertion here is a match it must still find. The Rust-side
# `tests/regex_prefilter_differential.rs` pins the same property against
# `MUTSU_REGEX_PREFILTER=off`; this file pins the behaviour a Raku program can
# actually observe.

plan 21;

# --- the motivating gap: a run of ordinary character classes --------------
# A single first-character set admits every isolated digit, but the second
# and third character of `\d\d\d` are just as necessary a condition as the
# first -- and a chain that got the offset arithmetic wrong would reject a
# real run of three digits, not merely fail to narrow an isolated one.
ok 'a1b22c333d' ~~ / \d\d\d /,        'three consecutive digits are found';
is ('a1b22c333d' ~~ / \d\d\d /).Str, '333', 'and the match text is the real run';
nok 'a1b22c4d' ~~ / \d\d\d /,         'no run of three digits is correctly a miss';
is ('a1b22c333d444e' ~~ m:g/ \d\d\d /).join(','), '333,444', 'a :g scan finds every run';

# --- a fixed-count repeat of a Unicode property ----------------------------
is ('AbCDEFg HIJK' ~~ / <:Lu> ** 4 /).Str, 'CDEF', 'the first run of four uppercase letters wins';

# --- two composite classes back to back ------------------------------------
is ('a1 b22 c3' ~~ / <+digit> <+digit> /).Str, '22', 'two consecutive digits via composite classes';

# --- the chain stops where the pattern stops being exact -------------------
is ('xxaycxxabxx' ~~ / 'ab' c? /).Str, 'ab', 'an optional atom after a pinned run still matches';
nok 'xxaycxx' ~~ / 'ab' c? /,          'and a subject with no "ab" at all still misses';

# --- alternation of literals: sound even where imprecise -------------------
is ('zaazabzbbz' ~~ m:g/ 'aa' | 'bb' /).join(','), 'aa,bb', 'both branches are found, and only real matches';
nok 'zabz' ~~ / 'aa' | 'bb' /, 'neither branch matches "ab" alone';

# --- `:i` and `:m` never extend the chain past their first character ------
is ('xxABCxx' ~~ / :i 'abc' /).Str, 'ABC', ':i still matches the whole literal';
is ("xcaf\x[e9]zz" ~~ / [:m 'cafe'] /).Str, "caf\x[e9]", 'a scoped :m still matches';

# --- a code block after a pinned run keeps running per position -----------
my $n = 0;
my $m = 'zzabzzabqq' ~~ / 'ab' { $n++ } 'q' /;
is $m.Str, 'abq', 'the second "ab", immediately followed by "q", matches';
ok $n > 1, 'the code block ran again at the first "ab" too, before failing there';

# --- a chain through a <subrule> -------------------------------------------
grammar Kw {
    token kw { 'aa' | 'bb' }
}
is ('xaazbby' ~~ / <Kw::kw> 'z' /).Str, 'aaz', 'a subrule then a literal narrows past the subrule';
nok 'xccz' ~~ / <Kw::kw> 'z' /, 'and a non-match through the subrule is still a miss';

# --- a bounded repeat-with-separator through a <subrule> -------------------
# `<Kw2::kw> ** 2..3 % ','` cannot be unrolled into flat literal tokens the
# way a plain literal repeat is (the subrule's own length is not known until
# match time), so the parser keeps ONE token whose quantifier carries the
# separator -- exactly the shape where naively concatenating two mandatory
# copies of the subrule's own chain would silently skip the comma between
# them and reject a real match.
grammar Kw2 {
    token kw { 'ab' }
}
nok 'xxabxx' ~~ / <Kw2::kw> ** 2..3 % ',' /, 'a single occurrence with no separator is a miss';
is ('xxab,abxx' ~~ / <Kw2::kw> ** 2..3 % ',' /).Str, 'ab,ab', 'two occurrences joined by the separator match';
is ('xxab,ab,abxx' ~~ / <Kw2::kw> ** 2..3 % ',' /).Str, 'ab,ab,ab', 'three occurrences also match';

# --- a class that can consume more than one codepoint stops the chain -----
# `\r\n` is one grapheme cluster, so a class matching `\n` can consume TWO
# characters at a `\r` -- the position right after such a class atom cannot
# be pinned, but the pattern must still match correctly either way.
is ("a\r\nbc" ~~ / <[\n a]> 'x' /).defined, False, 'no "x" follows the newline atom here';
ok "a\r\nxc" ~~ / <[\n a]> 'x' /, 'the newline atom is still followed correctly';
