# Regex: `\x[21,21]` and `\o[41,41]` are runs of codepoints

Inside a regex, a comma-separated list in `\x[…]` or `\o[…]` read as one
malformed number, so `/c \x[21,21] d/` never matched `c!!d`. It is now a run of
codepoints, as in a string, with whitespace allowed around each number. The run
is one atom, as a quoted string is: `\x[61,62]**2` matches `abab`, as in
rakudo. The bracket body is decoded by one helper
(`runtime/regex_parse_escape.rs`) for both radixes (#18737). Rakudo also
matches codepoint escapes exactly under `:i`, which mutsu does not yet do: that
is #18777.
