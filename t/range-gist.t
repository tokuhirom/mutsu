use Test;

# Range.gist is identical to Range.raku in Rakudo: it shows the range notation,
# numeric endpoints render plainly, string endpoints are quoted (`"a".."c"`),
# i64::MAX / Whatever endpoints render as Inf, and `0..^N` uses the `^N` short
# form. Previously mutsu's gist dropped the quotes on string endpoints, leaked
# `9223372036854775807` for `1..*`, and expanded `^5` to `0..^5`.

plan 13;

# String ranges keep quotes in gist (both `.gist` and the implicit say-gist).
is ("a" .. "c").gist, '"a".."c"', 'string Range .gist quotes endpoints';
is ("a" ^.. "c").gist, '"a"^.."c"', 'excl-min string Range .gist';
is ("a" .. "c", "x" .. "z").gist, '("a".."c" "x".."z")', 'list of string Ranges gist';

# Numeric ranges render plainly.
is (1 .. 3).gist, '1..3', 'int Range .gist';
is (1 ..^ 4).gist, '1..^4', 'excl-end int Range .gist';
is (1 ^.. 5).gist, '1^..5', 'excl-start int Range .gist';
is (1 ^..^ 5).gist, '1^..^5', 'excl-both int Range .gist';
is (1.5 .. 3.5).gist, '1.5..3.5', 'rational Range .gist';

# Infinite / Whatever endpoints render as Inf, not i64::MAX.
is (1 .. *).gist, '1..Inf', 'open-ended Range .gist renders Inf';

# 0..^N short form.
is (^5).gist, '^5', '^N short form in gist';

# gist == raku for ranges.
is ("a" .. "c").gist, ("a" .. "c").raku, 'Range gist matches raku (string)';
is (1 .. *).gist, (1 .. *).raku, 'Range gist matches raku (open-ended)';

# Range nested in a hash value gist keeps quotes.
is %(r => ("a" .. "c")).gist, '{r => "a".."c"}', 'Range in hash value gist';
