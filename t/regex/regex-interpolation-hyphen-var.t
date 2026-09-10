use Test;

plan 2;

my $full-path = 'line-number-cmp-ok.txt';
ok 'prefix line-number-cmp-ok.txt suffix' ~~ /$full-path/, 'regex interpolates scalar variable names with hyphens';
nok 'prefix line-number-cmp-ok suffix' ~~ /$full-path/, 'interpolated value still matches literally';
