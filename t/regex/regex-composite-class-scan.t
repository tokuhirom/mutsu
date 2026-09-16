use v6;
use Test;

plan 17;

# An UNANCHORED scan over a `<+a -b>` composite class (ADR-0099 Stage 1, #8272).
# The prefilter derives a first-character set for such an atom now instead of
# widening it to "anything", which means the scan hands the engine only the
# positions the class could start at. Every assertion below is a match the
# prefilter must not skip: an over-promising derivation drops a valid match
# silently, so these are the shapes each half of the derivation is responsible
# for.

# --- a positive built-in item admits exactly its members -------------------
is ~("zzz 9 zzz" ~~ / <+digit> /), '9', 'a positive built-in item finds its member';
nok ("zzz zzz" ~~ / <+digit> /).defined, 'and reports no match when it has none';

# --- a negative item subtracts, and must not subtract too much -------------
is ~("aeixou" ~~ / <[a..z] - [aeiou]> /), 'x', 'a subtraction keeps the surviving members';
is ("ABCDEF" ~~ m:g/ <+upper -[A B]> /).elems, 4, 'a built-in minus two letters keeps the rest';
is ~("zzz_zzz" ~~ / <+alpha -[a..z]> /), '_',
        '`<+alpha>` includes `_`, which the lowercase run does not subtract';

# --- an empty positive list means "any character" --------------------------
is (";qaq;b" ~~ m:g/ <-[;] - [q]> /).elems, 2,
        'a multi-part all-negative class starts from every character';

# --- non-ASCII is admitted wholesale, never enumerated ---------------------
is ~("ab\x[e9]cd" ~~ / <+alpha -[a..z]> /), "\x[e9]",
        'a non-ASCII member of a built-in class is still found';
nok ("\x[e9]\x[3a9]" ~~ / <[a..z] - [aeiou]> /).defined,
        'a class whose items are all ASCII rules out a non-ASCII subject';

# --- `\r\n` is one grapheme, and the class arm tests it as `\n` ------------
ok ("x\r\ny" ~~ / <+space -[ ]> /).defined,
        'a class holding `\n` matches at the `\r` that starts a CRLF cluster';

# --- `:i` expands the tested character, in both halves ---------------------
# Kept on a caseless positive item on purpose: mutsu's `:i` reading of a
# CASED built-in name inside a composite class diverges from rakudo's
# (#8498), which is a pre-existing engine question and not the scan's.
is ~("zzz 5 zzz" ~~ / :i <+digit -[0]> /), '5',
        'under `:i` a member the negative item does not name still matches';
nok ("zzz 0 zzz" ~~ / :i <+digit -[0]> /).defined,
        'and the negative item still subtracts its own member';
is ~("zzz F zzz" ~~ / :i <+xdigit -[a]> /), 'F',
        'under `:i` the positive item admits both cases of a hex letter';
nok ("zzz A zzz" ~~ / :i <+xdigit -[a]> /).defined,
        'and the negative item subtracts the other case of its member';

# --- a scan must find a match wherever it sits -----------------------------
my $pad = 'abcdefg' x 200;
is ~("{$pad}Q{$pad}" ~~ / <+upper -[A]> /), 'Q', 'a lone member deep in a long subject is found';
nok ("{$pad}A{$pad}" ~~ / <+upper -[A]> /).defined, 'and a subtracted one is not';

# --- a user rule of the same name makes the derivation decline -------------
# When the built-in predicate rejects, the engine may resolve a grammar token
# of that name against the remaining input, which is not a character set at
# all — so a name a rule answers to widens the atom back to every position.
# What must not change is the answer.
{
    my token upper { 'zzq' }
    is ~("--Q--" ~~ / <+upper -[7]> /), 'Q',
            'the built-in members still match with a rule of the same name in scope';
    nok ("--5--" ~~ / <+upper -[7]> /).defined,
            'and a non-member still does not';
}
