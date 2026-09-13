use Test;

# A regex literal is scanned by grapheme, not by codepoint (#8319).
#
# Rakudo's regex grammar works on NFG graphemes, so a cluster written as
# several codepoints -- `क्ष` is `क` U+0915 + virama U+094D + `ष` U+0937 --
# is ONE atom. mutsu used to scan the pattern one `char` at a time, which
# rejected the virama as an unrecognized metacharacter, and matched literals
# one codepoint at a time, which found the mark *inside* a grapheme.

plan 23;

my $ksha = "\x[0915]\x[094D]\x[0937]";

# The pattern parses at all: the combining mark is part of the literal
# grapheme, never a metacharacter of its own.
ok $ksha ~~ /क्ष/, 'a multi-codepoint grapheme is a literal atom';
ok "aक्षb" ~~ /क्ष/, 'and it matches in the middle of a subject';
is ~($ksha ~~ /(क्ष)/)[0], $ksha, 'the whole cluster is captured';

# The atom is the grapheme, so neither half of the cluster matches alone.
nok $ksha ~~ /क/, 'the base consonant alone does not match the cluster';
nok $ksha ~~ /क्/, 'base + virama alone does not match the cluster';
nok $ksha ~~ /ष/, 'the conjunct consonant alone does not match the cluster';

# The same, spelled with escapes: a mark is never an atom on its own.
nok "a\x[094D]b" ~~ /\x[094D]/, 'a mark inside a grapheme is not matchable alone';
nok "a\x[5B4]b" ~~ /a/, 'a literal does not match the base of a synthetic grapheme';
nok "a\x[5B4]b" ~~ /\x[5B4]/, 'nor does the mark of one match alone';
ok "a\x[5B4]b" ~~ /b/, 'a following plain grapheme still matches';
ok "\x[094D]x" ~~ /\x[094D]/, 'a leading mark IS its own grapheme, and matches';

# Adjacent literals are re-joined into the cluster they spell, whether they
# come from source characters, escapes, or a quoted literal.
ok "a\x[5B4]b" ~~ /a\x[5B4]/, 'a base and an escaped mark form one atom';
ok "abc\x[5B4]def" ~~ /abc \x[5B4] def/, 'insignificant space does not split the cluster';
ok $ksha ~~ /'क्ष'/, 'a quoted literal is scanned by grapheme too';
ok $ksha ~ $ksha ~~ /क्ष ** 2/, 'a quantifier applies to the whole grapheme';
ok $ksha ~ $ksha ~~ /क्ष+/, 'and so does a trailing +';

# NFG composition: the subject holds the composed spelling, so the pattern
# must be composed the same way.
ok "o\x[328]\x[304]" ~~ /o\x[328]\x[304]/, 'a decomposed cluster matches its NFC subject';
ok "cafe\x[301]" ~~ /café/, 'a precomposed pattern matches a decomposed source spelling';

# `\r\n` is a grapheme too, and the same re-joining keeps it whole.
my $crlf = "\r\n";
ok "a\r\nb" ~~ /$crlf/, 'an interpolated CRLF is re-joined into one atom';
ok "a\r\nb" ~~ /\n/, 'and the \n escape still matches that whole grapheme';

nok "abc\x[301]def" ~~ /abc/, 'a literal run stops at a synthetic grapheme';

# A control character ends its own cluster (UAX #29 GB4), so a mark after one
# is a grapheme of its own and the control still matches.
ok "\t\x[0300]" ~~ /\t/, 'a control is a grapheme even when a mark follows it';

# The Lang::Transliterate::Sa::IAST line that found this.
my $virama = "\x[094D]";
my $text = "क्\x[094D]ष";
is $text.subst(/क्$virama ष/, $ksha, :g), "क्" ~ $virama ~ "ष",
    'the substitution that motivated the ticket runs';
