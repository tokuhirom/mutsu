use Test;

plan 6;

# Allomorph.Str is the string part; the Int's :superscript/:subscript do not apply.
is <5>.Str(:superscript), '5', 'IntStr.Str(:superscript)';
is <5>.Str(:subscript), '5', 'IntStr.Str(:subscript)';
is <5.5>.Str(:superscript), '5.5', 'RatStr.Str(:superscript)';
is <5>.Str(:foo), '5', 'an unknown named argument is ignored';
is <5>.Str.^name, 'Str', 'plain .Str is still the string part';
is 5.Str(:superscript), '⁵', 'a plain Int still renders raised digits';

# vim: expandtab shiftwidth=4
