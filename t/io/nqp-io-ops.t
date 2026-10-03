use Test;
use nqp;

# The file-handle and filesystem nqp:: ops (#11501). Expected values and
# error messages were checked against rakudo.

plan 42;

my $dir = $*TMPDIR.add("mutsu-nqp-io-ops-{$*PID}");
$dir.mkdir;
my $file = $dir.add('a.txt').Str;
LEAVE { run 'rm', '-rf', $dir.Str }

sub dies-with(&code, $message, $desc) {
    my $got = '';
    { code(); CATCH { default { $got = .message } } }
    is $got, $message, $desc;
}

# nqp::say / nqp::print write to the process stdout and answer their operand.
{
    my $p = run $*EXECUTABLE, '-e',
        'use nqp; my $r := nqp::say("hi"); nqp::print("x\n"); nqp::print($r ~ "\n")', :out;
    is $p.out.slurp(:close), "hi\nx\nhi\n", 'nqp::say / nqp::print';
}

# Handle ops.
{
    is nqp::filenofh(nqp::getstdout()), 1, 'filenofh of stdout';
    is nqp::filenofh(nqp::getstderr()), 2, 'filenofh of stderr';
    my $fh := nqp::open($file, 'w');
    my $buf := "héllo\n".encode;
    ok nqp::writefh($fh, $buf) === $buf, 'writefh answers the buffer';
    is nqp::tellfh($fh), 7, 'tellfh counts bytes written';
    ok nqp::isnull(nqp::flushfh($fh)), 'flushfh answers null';
    dies-with { nqp::writefh($fh, "str") }, 'write_fhb requires a native array to read from',
        'writefh needs a buffer';
    nqp::closefh($fh);

    my $r := nqp::open($file, 'r');
    ok nqp::filenofh($r) > 2, 'filenofh of a file';
    is nqp::eoffh($r), 0, 'eoffh before reading';
    ok nqp::isnull(nqp::seekfh($r, 2, 0)), 'seekfh answers null';
    is nqp::tellfh($r), 2, 'seekfh from the start';
    my $b := buf8.new;
    nqp::readfh($r, $b, 100);
    is $b.list, (0xA9, 0x6C, 0x6C, 0x6F, 0x0A), 'read after seekfh';
    is nqp::eoffh($r), 1, 'eoffh at the end';
    nqp::seekfh($r, -1, 2);
    is nqp::tellfh($r), 6, 'seekfh from the end';
    nqp::seekfh($r, -2, 1);
    is nqp::tellfh($r), 4, 'seekfh from the current position';
    nqp::closefh($r);
    is nqp::filenofh($r), -1, 'filenofh of a closed handle';
    dies-with { nqp::getport(nqp::getstdout()) }, 'Cannot getport for this kind of handle',
        'getport of a non-socket';
}

# Directories.
{
    my $nested = $dir.add('x/y/z').Str;
    is nqp::mkdir($nested, 0o700), $nested, 'mkdir answers its path';
    ok $nested.IO.d, 'mkdir creates missing parents';
    is $dir.add('x').mode, '0700', 'mkdir applies the mode';
    is nqp::mkdir($nested, 0o700), $nested, 'mkdir of an existing directory';
    dies-with { nqp::mkdir("$file/sub", 0o755) }, 'Failed to mkdir: not a directory',
        'mkdir under a file';
    is nqp::rmdir($nested), $nested, 'rmdir answers its path';
    nqp::rmdir($dir.add('x/y').Str);
    nqp::rmdir($dir.add('x').Str);
    nok $dir.add('x').e, 'rmdir removed the directories';
    dies-with { nqp::rmdir($dir.add('nope').Str) }, 'Failed to rmdir: no such file or directory',
        'rmdir of a missing directory';
}

# Files and links.
{
    my $missing = $dir.add('nope').Str;
    is nqp::unlink($missing), $missing, 'unlink of a missing file is not an error';
    dies-with { nqp::unlink($dir.Str) }, 'Failed to delete file: illegal operation on a directory',
        'unlink of a directory';
    is nqp::chmod($file, 0o600), $file, 'chmod answers its path';
    is $file.IO.mode, '0600', 'chmod sets the mode';
    dies-with { nqp::chmod($missing, 0o600) },
        'Failed to set permissions on path: no such file or directory', 'chmod of a missing path';
    is nqp::chown($file, -1, -1), $file, 'chown with -1 ids changes nothing';

    my $hard = $dir.add('hard').Str;
    my $soft = $dir.add('soft').Str;
    ok nqp::isnull(nqp::link($file, $hard)), 'link answers null';
    ok nqp::isnull(nqp::symlink($file, $soft)), 'symlink answers null';
    ok $soft.IO.l && $hard.IO.f, 'link and symlink create the links';
    dies-with { nqp::link($file, $hard) }, 'Failed to link file: file already exists',
        'link onto an existing path';

    my $moved = $dir.add('moved').Str;
    is nqp::rename($file, $moved), $file, 'rename answers its source';
    nqp::rename($moved, $file);
    dies-with { nqp::rename($missing, $moved) }, 'Failed to rename file: no such file or directory',
        'rename of a missing file';
    my $copy = $dir.add('copy').Str;
    is nqp::copy($file, $copy), $file, 'copy answers its source';
    is $copy.IO.slurp, "héllo\n", 'copy copies the content';

    is nqp::filewritable($file), 1, 'filewritable';
    is nqp::fileexecutable($missing), 0, 'fileexecutable of a missing path';
    isa-ok nqp::stat_time($file, nqp::const::STAT_MODIFYTIME), Num, 'stat_time answers a Num';
}
