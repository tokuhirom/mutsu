use Test;

plan 15;

# Negative (and explicitly positive) numeric words in <...> must produce
# allomorphs (IntStr/RatStr/NumStr), not bare Str — matching positive words.

isa-ok <-3>,    IntStr, '<-3> is IntStr';
isa-ok <3>,     IntStr, '<3> is IntStr';
isa-ok <+7>,    IntStr, '<+7> is IntStr';
isa-ok <-3.5>,  RatStr, '<-3.5> is RatStr';
isa-ok <3.5>,   RatStr, '<3.5> is RatStr';
isa-ok <-1.2e3>, NumStr, '<-1.2e3> is NumStr';

# Numeric value is correct and negative.
is <-3> + 0,    -3,   '<-3> numifies to -3';
is <-3.5> + 0,  -3.5, '<-3.5> numifies to -3.5';
is <+7> + 0,    7,    '<+7> numifies to 7';

# Str component keeps the original signed spelling.
is <-3>.Str,   '-3',   '<-3>.Str keeps the sign';
is <-3.5>.Str, '-3.5', '<-3.5>.Str keeps the sign';

# Smartmatches against both the numeric type and Str.
ok <-3> ~~ Int, '<-3> smartmatches Int';
ok <-3> ~~ Str, '<-3> smartmatches Str';

# In a word list, all elements are allomorphs.
my @a = <5 -3 7 -9>;
ok @a.all ~~ IntStr, 'all words in <5 -3 7 -9> are IntStr';
is @a[1], -3, 'negative element value is correct';

# vim: expandtab shiftwidth=4
