use Test;
plan 6;

# Basic tr///
my $s1 = "hello";
$s1 ~~ tr/l/r/;
is $s1, 'herro', 'basic tr///';

# tr/// with ranges
my $s2 = "Hello World";
$s2 ~~ tr/a..z/A..Z/;
is $s2, 'HELLO WORLD', 'tr/// with ranges';

# tr/// multiple characters
my $s3 = "abcdef";
$s3 ~~ tr/ace/ACE/;
is $s3, 'AbCdEf', 'tr/// multiple chars';

# TR/// (same as tr///)
my $s4 = "hello";
$s4 ~~ TR/e/a/;
is $s4, 'hallo', 'TR///';

# tr/// with to shorter than from (last char repeats)
my $s5 = "abcde";
$s5 ~~ tr/abcde/XY/;
is $s5, 'XYYYY', 'tr/// with short replacement';

# tr/// no match
my $s6 = "hello";
$s6 ~~ tr/xyz/ABC/;
is $s6, 'hello', 'tr/// no match leaves string unchanged';

done-testing;
