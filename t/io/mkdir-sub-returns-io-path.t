use v6;
use Test;

# From App::RaCoCo (t/lib/TmpDir.rakumod): `sub create-dir(IO() $p --> IO::Path:D)
# { mkdir $p }` relies on the `mkdir` sub answering the directory's IO::Path
# (it is `IO::Path.new($path).mkdir`), and a failure answering a Failure
# rather than dying.

plan 7;

my $base = $*TMPDIR.child("mutsu-mkdir-sub-{$*PID}");
END { try { $base.child('a').rmdir; $base.rmdir } }

my $made = mkdir $base;
isa-ok $made, IO::Path, 'mkdir of a Str answers an IO::Path';
is $made.absolute, $base.absolute, 'for the directory made';
ok $base.d, 'and it exists';

my $again = mkdir $base.child('a');
isa-ok $again, IO::Path, 'mkdir of an IO::Path answers an IO::Path';

my sub typed(IO() $p --> IO::Path:D) { mkdir $p }
lives-ok { typed($base.child('a')) }, 'usable as the value of an IO::Path:D routine';

my $failed = mkdir '/proc/mutsu-no-such/dir';
isa-ok $failed, Failure, 'a failing mkdir answers a Failure';
$failed.so; # handled
ok $failed ~~ Failure, 'not thrown';

done-testing;
