use Test;

# A quantified built-in named char-class subrule (`<alpha>+`, `<digit>**N`)
# produces a LIST of Match objects -- one per repetition -- accessible via the
# named capture, matching Rakudo. (A user alias `$<x>=\w+` still captures the
# whole span as a single Match; that is exercised elsewhere.)

plan 18;

# <alpha>+ : one Match per matched char
ok "abc" ~~ /<alpha>+/, 'matched <alpha>+';
is $/.Str, 'abc', 'whole match spans the run';
isa-ok $<alpha>, Array, '$<alpha> is an Array for <alpha>+';
is $<alpha>.elems, 3, '<alpha>+ captured 3 entries';
is $<alpha>[0].Str, 'a', 'entry 0';
is $<alpha>[1].Str, 'b', 'entry 1';
is $<alpha>[2].Str, 'c', 'entry 2';

# <digit>+
ok "123" ~~ /<digit>+/, 'matched <digit>+';
is $<digit>.elems, 3, '<digit>+ captured 3 entries';
is $<digit>.map(*.Str).join('|'), '1|2|3', '<digit>+ entries in order';

# <alpha> ** 2 : counted quantifier also lists
ok "abcd" ~~ /<alpha> ** 2/, 'matched <alpha> ** 2';
is $<alpha>.elems, 2, '<alpha> ** 2 captured 2 entries';

# <alpha>* matching zero times -> empty Array, not Nil/Any
ok "" ~~ /<alpha>*/, '<alpha>* matches empty string';
isa-ok $<alpha>, Array, 'zero-match <alpha>* still yields an Array';
is $<alpha>.elems, 0, 'zero-match <alpha>* is an empty list';

# A single (unquantified) <alpha> remains a plain Match
ok "a" ~~ /<alpha>/, 'matched single <alpha>';
isa-ok $<alpha>, Match, 'single <alpha> is a Match';
is $<alpha>.Str, 'a', 'single <alpha> value';
