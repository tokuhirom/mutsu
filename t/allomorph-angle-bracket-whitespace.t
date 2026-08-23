use Test;

# `<...>` is fundamentally quote-words, and quote-words always produce the
# *allomorph* (IntStr / RatStr / NumStr / ComplexStr) for a number-shaped word.
# Raku's plain `Rat` and `Complex` come instead from two dedicated numeric
# *literal terms* in the grammar -- `rat_number` (`<nu/de>`) and
# `complex_number` (`<re±im i>`) -- which are recognised only when the bracket
# content is exactly that literal. Padding whitespace disqualifies the literal
# reading, so the very same number changes type:
#
#     <42/10>    Rat        < 42/10 >    RatStr
#     <1+42i>    Complex    < 1+42i >    ComplexStr
#
# mutsu used to shortcut a single space-padded fraction straight to a plain
# `Rat`, losing the allomorph.

plan 84;

# --- Int: allomorphic whether tight or padded --------------------------------

is <42>.^name,     'IntStr', '<42> is an IntStr';
is < 42 >.^name,   'IntStr', '< 42 > is an IntStr';
is <-42>.^name,    'IntStr', '<-42> is an IntStr';
is < -42 >.^name,  'IntStr', '< -42 > is an IntStr';
is <1_000>.^name,  'IntStr', '<1_000> is an IntStr';
is <0x1f>.^name,   'IntStr', '<0x1f> is an IntStr';
is < 0x1f >.^name, 'IntStr', '< 0x1f > is an IntStr';
is <0b101>.^name,  'IntStr', '<0b101> is an IntStr';
is +<42>, 42,      '<42> numeric half';
is ~< 42 >, '42',  '< 42 > string half keeps the source spelling';

# --- Rat written as a decimal: allomorphic whether tight or padded -----------

is <42.5>.^name,   'RatStr', '<42.5> is a RatStr';
is < 42.5 >.^name, 'RatStr', '< 42.5 > is a RatStr';
is <.5>.^name,     'RatStr', '<.5> is a RatStr';
is +<42.5>, 42.5,  '<42.5> numeric half';

# --- Rat written as a fraction: the literal term, so tight is a plain Rat ----
#     ...but a padded one is quote-words and stays allomorphic (the bug).

is <42/10>.^name,    'Rat',    '<42/10> is a plain Rat (rat_number literal)';
is < 42/10 >.^name,  'RatStr', '< 42/10 > is a RatStr (padded, so quote-words)';
is <2/3>.^name,      'Rat',    '<2/3> is a plain Rat';
is < 2/3 >.^name,    'RatStr', '< 2/3 > is a RatStr';
is <1/0>.^name,      'Rat',    '<1/0> is a plain Rat';
is < 1/0 >.^name,    'RatStr', '< 1/0 > is a RatStr';
is +< 42/10 >, 4.2,  '< 42/10 > numeric half is the divided value';
is ~< 42/10 >, '42/10', '< 42/10 > string half keeps the fraction spelling';
is < 42/10 >.numerator,   21, '< 42/10 > numerator (reduced)';
is < 42/10 >.denominator,  5, '< 42/10 > denominator (reduced)';
ok < 42/10 > == 42/10, '< 42/10 > compares numerically equal to the Rat';
ok < 42/10 > eq '42/10', '< 42/10 > compares stringwise to its spelling';

# The literal term is `signed-integer / integer`: the numerator may carry a
# sign but the denominator may not.
is <+1/2>.^name,  'Rat',    '<+1/2> is a plain Rat (signed numerator allowed)';
is <-1/2>.^name,  'Rat',    '<-1/2> is a plain Rat';
is <1/+3>.^name,  'RatStr', '<1/+3> is a RatStr (signed denominator is not a literal)';
is <+1/+3>.^name, 'RatStr', '<+1/+3> is a RatStr';

# Quote-word fractions divide arbitrary numeric parts. They are not literal
# terms, so even a tightly written decimal/exponent fraction is an allomorph.
is <1.5/2>.^name,   'RatStr', '<1.5/2> is a RatStr';
is +<1.5/2>,        0.75,    '<1.5/2> divides a decimal numerator';
is <1/2.5>.^name,   'RatStr', '<1/2.5> is a RatStr';
is +<1/2.5>,        0.4,     '<1/2.5> divides a decimal denominator';
is +<.5/2>,         0.25,    '<.5/2> accepts a leading-dot numerator';
is +<1/.5>,         2,       '<1/.5> accepts a leading-dot denominator';
is +<1/-3>,         -1/3,    '<1/-3> accepts a signed denominator';
is +<-1/-3>,        1/3,     '<-1/-3> accepts signed parts';
is <1e2/2>.^name,   'NumStr', '<1e2/2> promotes the result to NumStr';
is +<1e2/2>,        50e0,    '<1e2/2> performs Num division';
is <Inf/2>.^name,   'NumStr', '<Inf/2> is a NumStr';
is +<Inf/2>,        Inf,     '<Inf/2> divides Inf';
ok (+<NaN/2>).isNaN,          '<NaN/2> divides NaN';

# --- Num: allomorphic whether tight or padded --------------------------------

is <4e2>.^name,   'NumStr', '<4e2> is a NumStr';
is < 4e2 >.^name, 'NumStr', '< 4e2 > is a NumStr';
is <1E2>.^name,   'NumStr', '<1E2> is a NumStr';
is +<4e2>, 400e0, '<4e2> numeric half';

# Inf / NaN are Num-shaped words, so they are NumStr allomorphs too. The
# spellings are case-sensitive, and only Inf accepts a sign.
is <Inf>.^name,   'NumStr', '<Inf> is a NumStr';
is < Inf >.^name, 'NumStr', '< Inf > is a NumStr';
is <+Inf>.^name,  'NumStr', '<+Inf> is a NumStr';
is <-Inf>.^name,  'NumStr', '<-Inf> is a NumStr';
is <NaN>.^name,   'NumStr', '<NaN> is a NumStr';
is +<Inf>, Inf,   '<Inf> numeric half is Inf';
is +<-Inf>, -Inf, '<-Inf> numeric half is -Inf';
ok (+<NaN>).isNaN, '<NaN> numeric half is NaN';
is ~<Inf>, 'Inf',  '<Inf> string half';
is <inf>.^name,   'Str', '<inf> is a plain Str (case-sensitive)';
is <nan>.^name,   'Str', '<nan> is a plain Str (case-sensitive)';
is <-NaN>.^name,  'Str', '<-NaN> is a plain Str (NaN takes no sign)';

# --- Complex: the literal term needs both a real and an imaginary part -------

is <1+42i>.^name,    'Complex',    '<1+42i> is a plain Complex (complex_number literal)';
is < 1+42i >.^name,  'ComplexStr', '< 1+42i > is a ComplexStr (padded)';
is <3-3i>.^name,     'Complex',    '<3-3i> is a plain Complex';
is <0+42i>.^name,    'Complex',    '<0+42i> is a plain Complex';
is <3.5+2.1i>.^name, 'Complex',    '<3.5+2.1i> is a plain Complex';
is ~< 1+42i >, '1+42i', '< 1+42i > string half';

# A bare imaginary has no real part, so it never matches the literal term and
# stays a ComplexStr even when tight.
is <42i>.^name,   'ComplexStr', '<42i> is a ComplexStr (no real part)';
is < 42i >.^name, 'ComplexStr', '< 42i > is a ComplexStr';
is <+42i>.^name,  'ComplexStr', '<+42i> is a ComplexStr';
is <-42i>.^name,  'ComplexStr', '<-42i> is a ComplexStr';
is +<42i>, 42i,   '<42i> numeric half';

# U+2212 MINUS SIGN is accepted inside the literal terms.
is <5−1i>.^name, 'Complex', '<5-1i> with U+2212 is a plain Complex';
is <−1/2>.^name, 'Rat',     '<-1/2> with U+2212 is a plain Rat';

# --- Multi-word lists are always plain quote-words ---------------------------

is <1/2 3/4>.elems, 2, '<1/2 3/4> is a two-element list';
is <1/2 3/4>[0].^name, 'RatStr', 'a fraction in a word list is a RatStr';
is < 1/2 3/4 >[1].^name, 'RatStr', 'padding does not change a word list';
is <1+2i 3>[0].^name, 'ComplexStr', 'a complex in a word list is a ComplexStr';

# --- A colonpair value is a quote-words slot, never a literal term -----------

is (:a<2/3>).value.^name,   'RatStr',     ':a<2/3> value is a RatStr';
is (:a< 2/3 >).value.^name, 'RatStr',     ':a< 2/3 > value is a RatStr';
is (:a<1+2i>).value.^name,  'ComplexStr', ':a<1+2i> value is a ComplexStr';
is (:a<Inf>).value.^name,   'NumStr',     ':a<Inf> value is a NumStr';

# --- Non-numeric words stay plain Str ----------------------------------------

is <abc>.^name,   'Str', '<abc> is a Str';
is < abc >.^name, 'Str', '< abc > is a Str';
is <1+i>.^name,   'Str', '<1+i> is a Str (no imaginary magnitude)';
is <2/3/4>.^name, 'Str', '<2/3/4> is a Str';

done-testing;
