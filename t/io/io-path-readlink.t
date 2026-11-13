use Test;

# IO::Path.readlink answers a symlink's target: made absolute against
# $*CWD, with the link's directory (or the given CWD) as its CWD. A path that
# is not a symlink answers itself; a missing one is .l's Failure.

plan 7;

my $d = $*TMPDIR.add("mutsu-readlink-$*PID-{now.Num}");
$d.mkdir;
my $t = $d.add('t.txt');
$t.spurt('x');
$t.symlink($d.add('abs.txt'));
symlink('t.txt', $d.add('rel.txt').Str);

my $abs = $d.add('abs.txt').readlink;
isa-ok $abs, IO::Path, 'readlink answers an IO::Path';
is $abs.Str, $t.Str, 'an absolute target';
is $abs.CWD, $d.Str, 'its CWD is the link\'s directory';
is $d.add('rel.txt').readlink.Str, $*CWD.add('t.txt').Str,
    'a relative target is made absolute against $*CWD';
is $d.add('rel.txt').readlink('/foo').CWD, '/foo', 'an explicit CWD';
is $t.readlink.Str, $t.Str, 'a plain file answers itself';
my $missing = $d.add('missing').readlink;
ok $missing ~~ Failure && $missing.exception ~~ X::IO::DoesNotExist,
    'a missing path is an X::IO::DoesNotExist Failure';

END { run 'rm', '-rf', $d.absolute if $d.defined }
