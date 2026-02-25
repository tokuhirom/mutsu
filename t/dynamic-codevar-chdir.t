use Test;

plan :skip-all<requires $*TMPDIR directory> unless $*TMPDIR ~~ :d;
plan 2;

my $before = $*CWD;
my $after = &*chdir($*TMPDIR);

is $after.Str, $*TMPDIR.Str, '&*chdir returns new cwd';
is $*CWD.Str, $*TMPDIR.Str, '&*chdir updates dynamic cwd';

&*chdir($before);
