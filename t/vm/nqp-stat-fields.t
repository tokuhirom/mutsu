use v6;
use Test;
use nqp;

# Every MoarVM `STAT_*` field of `nqp::stat` / `nqp::lstat`, as File::Stat
# reads them.

plan 10;

my $file = 't/vm/nqp-stat-fields.t';
is nqp::stat($file, nqp::const::STAT_EXISTS), 1, 'STAT_EXISTS';
is nqp::stat($file, nqp::const::STAT_FILESIZE), $file.IO.s, 'STAT_FILESIZE';
is nqp::stat($file, nqp::const::STAT_ISREG), 1, 'STAT_ISREG';
is nqp::stat('t', nqp::const::STAT_ISDIR), 1, 'STAT_ISDIR';
is nqp::stat($file, nqp::const::STAT_MODIFYTIME), $file.IO.modified.to-posix[0].Int, 'STAT_MODIFYTIME';
is nqp::stat($file, nqp::const::STAT_PLATFORM_MODE) +& 0o170000, 0o100000,
    'STAT_PLATFORM_MODE carries the regular-file type bits';
ok nqp::stat($file, nqp::const::STAT_PLATFORM_NLINKS) >= 1, 'STAT_PLATFORM_NLINKS';
ok nqp::stat($file, nqp::const::STAT_PLATFORM_INODE) > 0, 'STAT_PLATFORM_INODE';
is nqp::lstat($file, nqp::const::STAT_ISLNK), 0, 'nqp::lstat STAT_ISLNK on a plain file';
is nqp::stat('no/such/file', nqp::const::STAT_EXISTS), 0, 'STAT_EXISTS on a missing path';
