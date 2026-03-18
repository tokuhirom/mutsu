use Test;

plan 4;

my $path = "/path//to///a//////file";
$path ~~ s:Perl5:g{/+} = '/';
is $path, "/path/to/a/file", 'smartmatch parses P5 substitution with = replacement';

my $rule = '\d+';
ok '2342' ~~ m:P5/$rule/, 'P5 match interpolates lexical variable';

my $rule2 = 'he(l)+o';
ok 'hello' ~~ m:P5/$rule2/, 'P5 match interpolates lexical variable with captures';

my $rule3 = 'r+';
my $bar = "barrrr";
$bar ~~ s:P5:g{$rule3} = 'z';
is $bar, "baz", 'P5 substitution interpolates lexical variable';
