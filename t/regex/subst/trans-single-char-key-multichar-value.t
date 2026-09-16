use v6;
use Test;

# `\r\n` is one grapheme cluster in Raku -- the same rule the regex engine's
# `can_start_a_longer_grapheme` applies -- so a replacement string built out of
# a literal "\r\n" is ONE "character" for `.trans`, not two. `.trans` used a
# raw per-codepoint split for a value string (needed for spec strings like
# `'a'..'z'`), which silently truncated a `"\r\n"` replacement down to just
# its `"\r"` when zipped against a single-character key. Found via
# LWP::Simple's ecosystem-parity hang, whose `q:to/END/.trans: ["\n" =>
# "\r\n"]` built its whole HTTP response with exactly this idiom -- every
# "\n" it wrote lost its trailing "\r", corrupting the "\r\n\r\n" header
# terminator the client then scanned for and never found.
#
# An ordinary (non-CRLF) multi-char replacement is NOT one grapheme, so a
# 1-char key still only takes its first character -- that is not a bug, and
# both mutsu and rakudo agree on it (verified against `raku`).

plan 4;

is "a\nb\nc".trans(["\n" => "\r\n"]), "a\r\nb\r\nc",
    "a 1-char key keeps a \\r\\n replacement whole (it is one grapheme)";
is "a\nb".trans(["\n" => "<br>"]), "a<b",
    "a 1-char key takes only the first char of an ordinary multi-char replacement";
is "aXbXc".trans(["X" => "--"]), "a-b-c",
    "same, for a non-newline 2-char replacement";

# The Str=>Str CYCLING form (a multi-char key) is unaffected by the grapheme
# handling above.
is "a123b123c".trans('123' => 'þð'), "aþðþbþðþc",
    "multi-char key still cycles a short replacement (regression guard)";

done-testing;
