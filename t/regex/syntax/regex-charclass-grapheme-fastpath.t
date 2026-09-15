use Test;

# The character-class atom used to build the NFC grapheme image of the subject
# at every position it was asked about, and then hand it to a comparison that
# only a `ClassItem::Grapheme` entry can consume. For `\w`, `\d`, `<[a..z]>` --
# every class without such an entry, which is nearly all of them -- that was a
# malloc, an NFC pass and a free per character tested, and nothing ever read
# the result: ~12% of the instructions of a failing `/ \w+ 'QQQ' /` scan
# (#8450).
#
# Skipping it when the class holds no grapheme entry is only sound if both
# sides keep behaving exactly as before, so pin both: a class that DOES hold a
# cluster still matches it, and the classes that now take the short path still
# respect grapheme boundaries. The `grapheme_end` / `is_grapheme_boundary`
# ASCII short-circuits added alongside are pinned by the same assertions --
# every cluster below must make them decline. Every expectation here was read
# off rakudo.

plan 15;

my $ksha = "\x[0915]\x[094D]\x[0937]";   # क + virama + ष, one grapheme

# --- a class that holds a multi-codepoint grapheme still matches it ---------

ok $ksha ~~ /<[क्ष]>/, 'an enumerated class matches a whole cluster';
is ~($ksha ~~ /(<[क्ष]>)/)[0], $ksha, 'and captures the whole cluster';
ok "aक्षb" ~~ /<[क्ष]>/, 'the cluster is found mid-subject';
nok "क" ~~ /<[क्ष]>/, 'the base consonant alone is not the cluster';
ok "aक्षb" ~~ /a <[क्ष]> b/, 'the cluster entry consumes exactly its own codepoints';

# A class holding both a cluster and ordinary members keeps both.
ok "z" ~~ /<[क्ष z]>/, 'a plain member of a cluster-bearing class still matches';
ok $ksha ~~ /<[क्ष z]>/, 'and the cluster member still matches too';
nok "q" ~~ /<[क्ष z]>/, 'a non-member is still rejected';

# --- classes with no grapheme entry keep their grapheme semantics -----------

# A class may not match the base of a synthetic grapheme, and may not start on
# the mark inside one.
nok "a\x[5B4]b" ~~ /<[a]>/, 'a class does not match the base of a synthetic grapheme';
nok "a\x[5B4]b" ~~ /<[\x[5B4]]>/, 'nor does its mark match alone';
ok "a\x[5B4]b" ~~ /<[b]>/, 'a following plain grapheme still matches';

# `\w` is the class on the hot path this change was made for. It must still
# consume a whole cluster rather than its base consonant.
is ~($ksha ~~ /\w/), $ksha, '`\w` consumes the whole cluster, not its base';

# A control ends its own cluster (UAX #29 GB4), so the ASCII short-circuit
# must not let `\t` swallow the mark that follows it.
is ~("\t\x[0300]" ~~ /\s/), "\t", 'a control is its own grapheme for a class too';

# `\r\n` is one grapheme, and a class that matches `\n` matches the pair.
ok "a\r\nb" ~~ /\v/, 'a vertical-space class matches the CRLF grapheme';

# The plain ASCII run that the short-circuits exist for still scans correctly.
is ~("abc def" ~~ / \w+ /), 'abc', 'an ASCII word run is matched whole';

# vim: expandtab shiftwidth=4
