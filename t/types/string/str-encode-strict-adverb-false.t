use Test;

# `.encode(:replacement)` picks between the lenient encode (substituting a
# replacement string/char for an unencodable codepoint) and the default
# strict one (throws). An explicit `:!replacement` -- a Pair whose value is
# `False` -- was treated like any OTHER present named argument: unconditionally
# stringified and used as the literal replacement text, so
# `.encode('ascii', :!replacement)` silently substituted bytes from the
# string "False" for every unencodable codepoint instead of throwing. It must
# behave exactly like omitting the adverb entirely.
#
# Found via Email::MIME's RFC 2231 filename-encoding logic (from the vendored
# Email::MIME distribution), which branches on whether
# `try { $value.encode('ascii', :!replacement) }` set `$!` to decide whether
# a Content-Disposition filename needs percent-encoding.

plan 6;

# No adverb at all: strict, throws.
dies-ok { "Ä".encode('ascii') }, 'encode(ascii) with no adverb throws on a non-ASCII codepoint';

# :!replacement must behave the same as no adverb: strict, throws.
dies-ok { "Ä".encode('ascii', :!replacement) },
    ':!replacement throws on a non-ASCII codepoint, same as omitting the adverb';

# An all-ASCII string never needs replacement either way.
is-deeply "Falstere".encode('ascii', :!replacement).list, "Falstere".encode('ascii').list.list,
    ':!replacement on an all-ASCII string matches the strict default';

# A truthy :replacement (Bool True) opts into the lenient '?' substitution.
is-deeply "A\x[c4]B".encode('ascii', :replacement).list, (65, 63, 66),
    ':replacement (True) substitutes the default "?" for an unencodable codepoint';

# An explicit replacement STRING is used verbatim.
is-deeply "A\x[c4]B".encode('ascii', replacement => 'XY').list, (65, 88, 89, 66),
    'an explicit :replacement string is used as the substitution text';

# iso-8859-1 goes through the same replacement-vs-strict switch.
dies-ok { "\x[100]".encode('iso-8859-1', :!replacement) },
    ':!replacement is strict for other encodings too (iso-8859-1)';
