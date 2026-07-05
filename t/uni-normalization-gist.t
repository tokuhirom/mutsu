use Test;

# The .NFC/.NFD/.NFKC/.NFKD methods return a Uni in the given normalization
# form. Its gist (and hence `say`) is `FORM:0x<codepoints>`, and its .raku
# appends the form — mutsu previously rendered the plain decoded text.

plan 12;

# gist / say
is "ﬀ".NFKC.gist, 'NFKC:0x<0066 0066>', 'NFKC gist shows form + codepoints';
is "①".NFKC.gist, 'NFKC:0x<0031>', 'NFKC gist of circled one';
is "café".NFD.gist, 'NFD:0x<0063 0061 0066 0065 0301>', 'NFD gist decomposes é';
is "a".NFC.gist, 'NFC:0x<0061>', 'NFC gist';
is "a".NFKD.gist, 'NFKD:0x<0061>', 'NFKD gist';

# .raku appends the form
is "ﬀ".NFKC.raku, 'Uni.new(0x0066, 0x0066).NFKC', 'NFKC raku appends form';
is "a".NFC.raku, 'Uni.new(0x0061).NFC', 'NFC raku appends form';

# .Str still returns the decoded text (print uses .Str)
is "ﬀ".NFKC.Str, 'ff', 'NFKC Str is decoded text';
is "café".NFD.Str, 'café', 'NFD Str is decoded text';

# Type name and codepoint methods are unchanged
is "ﬀ".NFKC.^name, 'NFKC', 'NFKC type name';
is "ﬀ".NFKC.codes, 2, 'NFKC codes counts codepoints';

# Inside an aggregate, the element gists with its form too
is ["a".NFC, "b".NFD].gist, '[NFC:0x<0061> NFD:0x<0062>]', 'Uni elements gist with form';
