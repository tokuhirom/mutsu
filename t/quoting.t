use Test;
plan 8;

# q// literal string (no interpolation)
my $q1 = q/hello world/;
is $q1, 'hello world', 'q// basic string';

# q() with parens
my $q2 = q(hello parens);
is $q2, 'hello parens', 'q() basic string';

# q{} with braces
my $q3 = q{hello braces};
is $q3, 'hello braces', 'q{} basic string';

# q// no interpolation
my $name = 'Raku';
my $q4 = q/hello $name/;
is $q4, 'hello $name', 'q// does not interpolate';

# qq// with interpolation
my $lang = 'Raku';
my $qq1 = qq/hello $lang/;
is $qq1, 'hello Raku', 'qq// interpolates variables';

# qq() with parens
my $qq2 = qq(hello $lang);
is $qq2, 'hello Raku', 'qq() interpolates variables';

# qq{} with braces
my $qq3 = qq{hello $lang};
is $qq3, 'hello Raku', 'qq{} interpolates variables';

# qq[] with brackets
my $qq4 = qq[hello $lang];
is $qq4, 'hello Raku', 'qq[] interpolates variables';
