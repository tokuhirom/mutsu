use Test;

plan 9;

# `Str.raku` writes a grapheme that would not round-trip as its codepoints
# (measured on rakudo 2026.11): one led by a Prepend codepoint anywhere, one
# led by a combiner with no base before it.
is "\x[600]".raku, '"\x[600]"', 'a lone Prepend codepoint';
is "a\x[600]b".raku, '"a\x[600,62]"', 'a Prepend joins the next codepoint';
is "x\x[600]".raku, '"x\x[600]"', 'a Prepend at the end';
is "\x[D01,D00]".raku, '"\x[D01,D00]"', 'a leading combiner grapheme, whole';
is "\x[301,302]".raku, '"\x[301,302]"', 'two leading combiners';
is "\n\x[301]".raku, '"\n\x[301]"', 'a combiner after a control character';
is "a\x[301]".raku, "\"a\x[301]\"", 'a combiner on a base stays as written';
is "a\x[200D]b".raku, "\"a\x[200D]b\"", 'a ZWJ after a base stays as written';
is "\r\n".raku, '"\r\n"', 'CRLF';
