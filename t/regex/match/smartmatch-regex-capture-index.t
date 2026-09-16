use Test;

# App::Prove6 / Getopt::Long indexes a capture directly from a smartmatch RHS.
my ($name) = 'option' ~~ / ^ (\w+) /[0];
is ~$name, 'option', 'a smartmatch regex result accepts positional capture indexing';

my $capture = 'value' ~~ / ^ (\w+) /[0];
is ~$capture, 'value', 'postcircumfix indexing works without destructuring too';

done-testing;
