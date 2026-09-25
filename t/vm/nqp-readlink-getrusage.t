use Test;
use nqp;

# #9348: nqp::readlink and nqp::getrusage. Expected shapes measured with
# rakudo 2026.07 (MoarVM's field order and libuv's error wording).

plan 9;

my $dir = $*TMPDIR.add("mutsu-readlink-$*PID-{now.Rat}");
$dir.mkdir;
my $target = $dir.add('target');
$target.spurt('x');
my $link = $dir.add('link');
$target.symlink($link);

is nqp::readlink(~$link), ~$target, 'readlink answers the symlink target';
throws-like { nqp::readlink(~$target) }, X::AdHoc,
    message => 'Failed to readlink file: invalid argument', 'a regular file is not a link';
throws-like { nqp::readlink(~$dir.add('missing')) }, X::AdHoc,
    message => 'Failed to readlink file: no such file or directory', 'a missing path';
$link.unlink; $target.unlink; $dir.rmdir;

my $r := nqp::list_i();
my $back := nqp::getrusage($r);
is nqp::elems($r), 18, 'getrusage fills the 18 rusage fields';
ok nqp::isnull($back), 'and answers null, like MoarVM';
my $burn = 0; $burn += $_ for ^20000;
ok nqp::atpos_i($r, 0) >= 0, 'user seconds';
ok 0 <= nqp::atpos_i($r, 1) < 1_000_000, 'user microseconds';
ok 0 <= nqp::atpos_i($r, 3) < 1_000_000, 'system microseconds';
ok nqp::atpos_i($r, 4) > 0, 'maxrss is positive';
