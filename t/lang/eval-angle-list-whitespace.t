use Test;
plan 4;

my @space = EVAL('<a b c>');
is @space, <a b c>, 'EVAL angle list splits on regular spaces';

my $nbsp = "\x[00A0]";
my $code = '<a' ~ $nbsp ~ 'b' ~ $nbsp ~ 'c>';
my @nbsp = EVAL($code);
is @nbsp, ['a' ~ $nbsp ~ 'b' ~ $nbsp ~ 'c'], 'EVAL angle list does not split on non-breaking spaces';
is ord(@nbsp[0].substr(1, 1)), 160, 'first non-breaking space is preserved';
is ord(@nbsp[0].substr(3, 1)), 160, 'second non-breaking space is preserved';
