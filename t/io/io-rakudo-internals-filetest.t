use Test;

plan 22;

my $dir = $*TMPDIR.add("filetest-{$*PID}-{(^1_000_000).pick}");
$dir.mkdir;
LEAVE { try { $dir.add('f.txt').unlink; $dir.add('l.txt').unlink; $dir.rmdir } }
$dir.add('f.txt').spurt("hi\n");
symlink 'f.txt', $dir.add('l.txt').Str;
my ($f, $d, $l, $none) = ($dir.add('f.txt'), $dir, $dir.add('l.txt'), $dir.add('none')).map(*.absolute);

# `Rakudo::Internals.FILETEST-*` answer an Int, and share their queries with IO::Path.
is Rakudo::Internals.FILETEST-E($f), 1, 'E of a file';
is Rakudo::Internals.FILETEST-E($none), 0, 'E of a missing path is 0, not an error';
is Rakudo::Internals.FILETEST-D($d), 1, 'D of a directory';
is Rakudo::Internals.FILETEST-D($f), 0, 'D of a file';
is Rakudo::Internals.FILETEST-F($f), 1, 'F of a file';
is Rakudo::Internals.FILETEST-L($l), 1, 'L of a symlink';
is Rakudo::Internals.FILETEST-L($f), 0, 'L of a plain file';
is Rakudo::Internals.FILETEST-F($l), 1, 'F follows a symlink';
is Rakudo::Internals.FILETEST-LE($l), 1, 'LE of a symlink';
is Rakudo::Internals.FILETEST-R($f), 1, 'R';
is Rakudo::Internals.FILETEST-RW($f), 1, 'RW';
is Rakudo::Internals.FILETEST-X($f), 0, 'X of a non-executable file';
is Rakudo::Internals.FILETEST-S($f), 3, 'S is the size';
is Rakudo::Internals.FILETEST-ES($none), 0, 'ES of a missing path is 0';
is Rakudo::Internals.FILETEST-Z($f), 0, 'Z of a non-empty file';
is Rakudo::Internals.FILETEST-MODE($f), $f.IO.mode.Int +& 0o7777, 'MODE is the permission bits';
is Rakudo::Internals.FILETEST-MODIFIED($f), $f.IO.modified.to-posix[0].Num, 'MODIFIED is the epoch seconds';
ok Rakudo::Internals.FILETEST-CHANGED($f) > 0, 'CHANGED';
ok Rakudo::Internals.FILETEST-ACCESSED($f) > 0, 'ACCESSED';
ok Rakudo::Internals.FILETEST-CREATED($f) > 0, 'CREATED';
throws-like { Rakudo::Internals.FILETEST-D($none) }, Exception, message => /'Failed to stat file'/, 'D of a missing path dies';
throws-like { Rakudo::Internals.FILETEST-S($none) }, Exception, message => /'Failed to stat file'/, 'S of a missing path dies';
