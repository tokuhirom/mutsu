use Test;

# A failed `link` / `symlink` (sub and IO::Path method) reports Rakudo's
# X::IO::Link / X::IO::Symlink: absolute paths and the libuv-worded
# os-error (#11737).

plan 10;

my $dir = $*TMPDIR.child("x-io-link-{$*PID}");
$dir.mkdir;
my $f = $dir.child('f.txt');
$f.spurt('x');
my $abs = $f.absolute;

indir $dir, {
    my $e = link('f.txt', 'f.txt').exception;
    isa-ok $e, X::IO::Link, 'link failure is X::IO::Link';
    is $e.message,
        "Failed to create link called '$abs' on target '$abs': Failed to link file: file already exists",
        'link message names both absolute paths and the os-error';
    is $e.target, $abs, 'target attribute is the absolute path';
    is $e.os-error, 'Failed to link file: file already exists', 'os-error attribute';

    $e = symlink('f.txt', 'f.txt').exception;
    isa-ok $e, X::IO::Symlink, 'symlink failure is X::IO::Symlink';
    is $e.message,
        "Failed to create symlink called '$abs' on target '$abs': Failed to symlink file: file already exists",
        'symlink message';
    is symlink('../x/./y', 'f.txt').exception.message,
        "Failed to create symlink called '$abs' on target '{$dir.absolute}/../x/y': Failed to symlink file: file already exists",
        'a relative symlink target is reported absolute, with "." segments dropped';

    is 'f.txt'.IO.link('f.txt').exception.message,
        "Failed to create link called '$abs' on target '$abs': Failed to link file: file already exists",
        'IO::Path.link';
    is 'f.txt'.IO.symlink('f.txt').exception.message,
        "Failed to create symlink called '$abs' on target '$abs': Failed to symlink file: file already exists",
        'IO::Path.symlink';
    is link('nope', 'g').exception.message,
        "Failed to create link called '{$dir.absolute}/g' on target '{$dir.absolute}/nope': Failed to link file: no such file or directory",
        'a missing target';
}

$f.unlink;
$dir.rmdir;
