use Test;

plan 1;

my $script = '_test_assertion_line_number.raku';
spurt $script, q:to/END/;
use Test;
plan 1;
sub foo-ok() is test-assertion { flunk "foo-ok" }
foo-ok;
END
my $proc = run($*EXECUTABLE, $script, :!out, :err);
my $err := $proc.err.slurp;
like $err,
    /'Failed test ' (\N* \n \N*)? 'at ' $script ' line ' 4/,
    'test-assertion failures report caller line';
unlink $script;
