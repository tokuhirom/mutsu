use Test;

# A failed chdir names the resolved absolute path, as rakudo does
# (Language/io-guide.rakudoc).
plan 4;

my $missing = 'no-such-dir-for-chdir-test';
my $abs = $*CWD.add($missing).absolute;

my $r = chdir $missing;
ok $r ~~ Failure, 'a relative path that does not exist fails';
is $r.exception.message, "Failed to change the working directory to '$abs': does not exist",
    'the message names the absolute path';
$r.so;   # handle it

my $r2 = chdir "/no-such-dir-for-chdir-test";
is $r2.exception.message, "Failed to change the working directory to '/no-such-dir-for-chdir-test': does not exist",
    'an absolute path is unchanged';
$r2.so;

my $r3 = chdir $*PROGRAM.absolute;
like $r3.exception.message, /"is not a directory"/, 'a file is not a directory';
$r3.so;
