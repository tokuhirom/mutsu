use Test;
# A Uni is a Positional[uint32]. Its no-argument join therefore joins the
# numeric codepoints, rather than stringifying the whole Uni. This is used by
# Text::MathematicalCase while building its case-conversion maps.
plan 2;
is 'abc'.NFD.join, '979899', 'a Uni joins its codepoints with the default separator';
is 'A'.NFC.join, '65', 'a single-codepoint Uni still joins numerically';
