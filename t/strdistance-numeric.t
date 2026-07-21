use v6;
use Test;

plan 6;

# A StrDistance (`$str ~~ tr/a/b/`) stringifies to the after-string and numifies
# to the edit distance between before and after. `+$sd` / `.Numeric` / `.Int`
# were returning 0 instead of the number of changes.
my $str = "fold";
my $sd = ($str ~~ tr/old/new/);
is $sd.^name, 'StrDistance', 'tr/// in smartmatch yields a StrDistance';
is ~$sd, 'fnew', 'StrDistance stringifies to the after-string';
is +$sd, 3, 'prefix + gives the edit distance';
is $sd.Numeric, 3, '.Numeric gives the edit distance';
is $sd.Int, 3, '.Int gives the edit distance';

# No changes -> distance 0
my $s2 = "abc";
my $sd2 = ($s2 ~~ tr/xyz/pqr/);
is +$sd2, 0, 'no matching chars -> distance 0';
