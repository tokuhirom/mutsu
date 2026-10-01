use Test;

# Unicode property tests no longer go through the `regex` crate (#10439):
# `\p{..}` classes resolve to codepoint ranges once, and a `<:name(...)>`
# argument is smartmatched as Rakudo does -- a regex runs on mutsu's own
# regex engine, a string compares for equality.

plan 18;

# Classes that `regex-syntax` folds to a single codepoint.
ok "\x2028" ~~ /<:Zl>/, 'Zl (one codepoint) matches U+2028';
ok "\x2029" ~~ /<:Zp>/, 'Zp (one codepoint) matches U+2029';
is "\n".uniprop('Grapheme_Cluster_Break'), 'LF', 'GCB of a newline is LF';
is "\r".uniprop('Grapheme_Cluster_Break'), 'CR', 'GCB of a carriage return is CR';

# Value-typed and binary properties.
ok 'a' ~~ /<:sc<Latin>>/, 'Script=Latin';
nok 'a' ~~ /<:sc<Greek>>/, 'Script=Greek does not match a Latin letter';
ok "\x1F600".uniprop('Emoji'), 'Emoji is a binary property';
is "\x0663".uniprop('Numeric_Type'), 'Decimal', 'Arabic-Indic digit is Decimal';

# `<:name(...)>`
is ('FooBar' ~~ /<:name(/:s LATIN SMALL LETTER/)>+/).Str, 'oo', 'name regex with :s';
is ('FooBar' ~~ /<:Name(/:s LATIN CAPITAL LETTER/)>+/).Str, 'F', 'Name regex';
is ('ab1' ~~ /<:name(/^DIGIT/)>/).Str, '1', 'name regex anchors at the start of the name';
is ('xA' ~~ /<!:name(/SMALL/)>./).Str, 'A', 'negated name regex';
is ('aB' ~~ /<:name("LATIN SMALL LETTER A")>/).Str, 'a', 'name string matches the whole name';
nok 'aB' ~~ /<:name("LATIN")>/, 'name string does not match a part of the name';

# The same tests as items of a combined class.
is ('aB' ~~ /<+:name(/CAPITAL/)>/).Str, 'B', 'name regex as a class item';
is ('aB' ~~ /<+:Lu +:name(/SMALL/)>+/).Str, 'aB', 'name regex in a class union';
is ('aB' ~~ /<-:name(/SMALL/)>/).Str, 'B', 'negated name-regex class';
is ('aB1' ~~ /<+:L -:name(/SMALL/)>/).Str, 'B', 'name regex subtracted from a class';
