use Test;
use nqp;

plan 3;

# After a stat call, `IO::Path`'s `$!os-path` is the absolute path rakudo caches
# (inode-dev-devtype reads it that way).
sub os-path($io) { nqp::getattr(nqp::decont($io), IO::Path, '$!os-path') }

my $f = $*PROGRAM.IO;
Rakudo::Internals.FILETEST-E($f.absolute);
is os-path($f), $f.absolute, 'the absolute path of a relative or absolute program path';
my $rel = "t".IO;
Rakudo::Internals.FILETEST-E($rel.absolute);
is os-path($rel), $rel.absolute, 'a relative path is made absolute';
is nqp::stat(os-path($f), nqp::const::STAT_PLATFORM_INODE), $f.inode, 'nqp::stat of it matches IO::Path.inode';
