use Test;

plan 6;

# Str.Stringy is self, and an allomorph is a Str: .Stringy answers the allomorph.
is <1>.Stringy.^name, 'IntStr', 'IntStr.Stringy keeps the allomorph';
is <1.5e0>.Stringy.^name, 'NumStr', 'NumStr.Stringy keeps the allomorph';
is <1.5e0>.Stringy, '1.5e0', 'the string part is unchanged';
is <1>.Stringy.raku, 'IntStr.new(1, "1")', '.raku shows both parts';
is "abc".Stringy.^name, 'Str', 'a plain Str stays a Str';
is <1>.Str.^name, 'Str', '.Str still answers the string part';

# vim: expandtab shiftwidth=4
