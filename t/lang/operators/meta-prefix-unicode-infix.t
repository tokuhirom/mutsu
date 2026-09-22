use Test;

# #9034: a Unicode operator alias parsed as a plain infix (`1 × 2`), inside a
# reduction (`[×] @a`) and inside hyper delimiters (`@a >>×<< @b`), but not
# after a `Z`/`X`/`R` meta prefix -- the meta-operator scanner's symbolic
# operator table was ASCII-only, so the parser stopped dead at the non-ASCII
# byte and `@a Z× @b` was a syntax error.
#
# A second, quieter half of the same gap: `[Z×]` parsed but then died
# "Unknown function: infix:<Z×>", because `is_builtin_infix` did not fold the
# alias. The bare `[×]` escaped that only because the reduction decoder folds
# the whole spelling, which leaves the alias untouched once a meta prefix sits
# in front of it.
#
# The rule these pin is that a Unicode alias works wherever its ASCII spelling
# does. Each assertion below therefore has its ASCII twin checked alongside it
# where the two are meant to agree.

plan 20;

my @a = 1, 2;
my @b = 3, 4;

# --- Zip / cross / reverse over a Unicode infix. ---
is (@a Z× @b).raku, '(3, 8).Seq', 'Z× zips the Unicode multiply alias';
is (@a Z* @b).raku, '(3, 8).Seq', 'Z* (the ASCII twin) still agrees';
is (@a X× @b).raku, '(3, 4, 6, 8).Seq', 'X× crosses the Unicode multiply alias';
is (@a X* @b).raku, '(3, 4, 6, 8).Seq', 'X* (the ASCII twin) still agrees';

is ((4, 9) Z÷ (2, 3)).raku, '(2.0, 3.0).Seq', 'Z÷ zips the Unicode divide alias';
is ((1, 2) Z− (0, 1)).List, (1, 1).List, 'Z− zips the U+2212 minus alias';

is 4 R× 3, 12, 'R× reverses the Unicode multiply alias';
is 4 R− 3, -1, 'R− reverses the U+2212 minus alias';

# --- Comparison aliases after a meta prefix. ---
is ((1, 2) Z≤ (2, 1)).raku, '(Bool::True, Bool::False).Seq', 'Z≤ zips the Unicode <= alias';
is ((1, 2) Z<= (2, 1)).raku, '(Bool::True, Bool::False).Seq', 'Z<= (the ASCII twin) still agrees';
is ((1, 2) X≥ (2, 1)).raku, '(Bool::False, Bool::True, Bool::True, Bool::True).Seq',
    'X≥ crosses the Unicode >= alias';
is ((1, 2) Z≠ (2, 2)).raku, '(Bool::True, Bool::False).Seq', 'Z≠ zips the Unicode != alias';

# --- A meta-assignment over a Unicode infix. ---
{
    my @c = 1, 2;
    @c Z×= (10, 20);
    is-deeply @c, [10, 40], 'Z×= accepts the Unicode alias as its inner op';
}

# --- Reduction OVER a meta-operator whose inner op is a Unicode alias. This
# is the `is_builtin_infix` half: `[Z*]` worked while `[Z×]` did not. ---
is ([Z×] ((1, 2), (3, 4))).List, (3, 8).List, '[Z×] reduces a zip over the Unicode alias';
is ([Z*] ((1, 2), (3, 4))).List, (3, 8).List, '[Z*] (the ASCII twin) still agrees';
is ([X×] ((1, 2), (3, 4))).List, (3, 4, 6, 8).List, '[X×] reduces a cross over the Unicode alias';
is ([R×] (2, 3)), 6, '[R×] reduces a reverse over the Unicode alias';

# --- The spellings that already worked must keep working. ---
is 2 × 3, 6, 'a bare Unicode infix still parses';
is ([×] (1, 2, 3)), 6, 'a plain reduction over a Unicode infix still parses';
is (@a >>×<< @b).raku, '[3, 8]', 'a hyper Unicode infix still parses';
