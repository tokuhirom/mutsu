use Test;

plan 6;

# A Str mixed into a value wins for .gist and .raku, as in Rakudo.
is (5 but "x").gist, "x", 'Int but Str: .gist is the Str';
is (5 but "x").raku, "x", 'Int but Str: .raku is the Str';
is (5 but "x").Str, "x", 'Int but Str: .Str is the Str';
# A non-Str mixin does not.
is (5 but 7).gist, "5", 'Int but Int: .gist is the inner value';
is ("a" but 5).raku, '"a"', 'Str but Int: .raku is the inner value';
# Allomorphs keep their own rendering.
is <5>.raku, 'IntStr.new(5, "5")', 'allomorph .raku unchanged';
