use Test;

plan 26;

# Numifying a string with a "lone i" — an imaginary unit written without an
# explicit coefficient — fails, as in Rakudo (#9731). Roast's S32-str/numeric.t
# asks for `+'−i'` to be `-i`, but fences those assertions with
# `#?rakudo skip 'cannot handle lone i yet'`; mutsu follows what Rakudo does,
# which real code (Text::SubParsers) relies on.

for <i +i -i −i 3+i 3-i −10−i \i 3+\i> -> $s {
    nok (try +$s.Str).defined, "'$s' does not numify (no coefficient)";
}

# `Inf`/`NaN` as the imaginary part need the `\i` spelling.
nok (try +'Infi').defined,   "'Infi' does not numify";
nok (try +'NaNi').defined,   "'NaNi' does not numify";
nok (try +'3+Infi').defined, "'3+Infi' does not numify";
is-deeply +'Inf\i',   Complex.new(0, Inf), "'Inf\\i' numifies";
is-deeply +'3+Inf\i', Complex.new(3, Inf), "'3+Inf\\i' numifies";

# Explicit coefficients still work unchanged.
is-deeply +'10i',   0+10i, 'number with i (coefficient present)';
is-deeply +'3+2i',  3+2i,  'both parts with coefficients';
is-deeply +'1+2\i', 1+2i,  'backslash i with coefficient';
nok (try +'0--1i').defined, 'a doubled imaginary sign does not numify';
ok (+'NaN+0i').re.isNaN, 'a NaN real part numifies';

# Complex cmp Str compares as strings, like Rakudo.
is '0+NaNi' cmp 0i, More, 'Str cmp Complex is a string comparison';

# Quote-words share the same parser: a lone i or `Infi` is a plain Str,
# while a `\i` Complex is an allomorph (or a Complex literal term).
isa-ok <x i>[1],       Str,        '<i> is a Str';
nok <x Infi>[1] ~~ Numeric,        '<Infi> is not numeric';
nok <x infi>[1] ~~ Numeric,        '<infi> is not numeric';
isa-ok <x Inf\i>[1],   ComplexStr, '<Inf\i> is a ComplexStr';
isa-ok <3+Inf\i>,      Complex,    '<3+Inf\i> is a Complex literal';

# The Text::SubParsers reduction: a word containing "i" must not numify.
my $input = 'The average mass is 55 lbs.';
my &func = { $_.trim ?? $_.trim.Numeric !! Nil };
my @found = ($input ~~ m:g/ (.+) <?{ my $p; try { $p = &func($0.Str) }; $p.defined && !$! }> /).map(~*);
is-deeply @found, [' 55 '], 'only the number matches, not a lone i';
