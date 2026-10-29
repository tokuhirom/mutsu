use Test;

plan 4;

my $start = $*CWD;
my $dir = $*TMPDIR.add("mutsu-chdir-sub-{$*PID}-{(^1000000).pick}");
$dir.mkdir;
LEAVE $dir.rmdir;

sub change-then-call($d) { chdir($d); sink 1.so; True }
sub change-via-code-var($d) { so &CORE::chdir($d) }
sub change-via-so($d) { so chdir($d) }

change-then-call($dir);
is $*CWD.resolve.Str, $dir.resolve.Str, 'chdir followed by another call in the sub reaches the caller';
$*CWD = $start;

change-via-code-var($dir);
is $*CWD.resolve.Str, $dir.resolve.Str, '&CORE::chdir inside a sub reaches the caller';
$*CWD = $start;

change-via-so($dir);
is $*CWD.resolve.Str, $dir.resolve.Str, 'chdir wrapped in a method call reaches the caller';
$*CWD = $start;

{
    my $inner = sub { chdir($dir); 42 }();
    is $*CWD.resolve.Str, $dir.resolve.Str, 'chdir in an anonymous sub reaches the caller';
}
$*CWD = $start;
