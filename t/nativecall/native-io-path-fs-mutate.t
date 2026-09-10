use Test;

# Pin for VM-native single-path `IO::Path` filesystem mutations (ledger §D):
# `.spurt` / `.mkdir` / `.rmdir` / `.unlink` / `.chmod`. Each resolves the path
# against the VM-owned cwd and performs a one-shot syscall, allocating no
# io_handles (`.spurt` opens, writes, and immediately drops its file handle), so
# the VM dispatches them natively via `try_io_path_fs_mutate` — the single impl
# `native_io_path` also delegates to. Both literal (`"x".IO.spurt(...)`, non-mut
# path) and variable (`$p.spurt(...)` -> CallMethodMut, mut path) receivers are
# exercised. Two-path ops (copy/rename/move/symlink/link) and handle-opening
# `open` stay in the interpreter and are intentionally not covered here.

plan 22;

my $dir = "tmp/native-io-mutate-$*PID";
mkdir $dir;

# --- .spurt (write) + round-trip via .slurp ---
my $f = "$dir/out.txt";
is $f.IO.spurt("hello\n"), True, '.spurt returns True';
is $f.IO.slurp, "hello\n",        '.spurt wrote the content';

# .spurt(:append)
$f.IO.spurt("more\n", :append);
is $f.IO.slurp, "hello\nmore\n",  '.spurt(:append) appends';

# .spurt(:createonly) on an existing file fails (Failure, not exception)
nok $f.IO.spurt("nope", :createonly).defined,
    '.spurt(:createonly) on an existing file is an undefined Failure';
is $f.IO.slurp, "hello\nmore\n",  '.spurt(:createonly) did not overwrite';

# .spurt a Buf writes raw bytes
my $bf = "$dir/bytes.bin";
$bf.IO.spurt(Buf.new(0x41, 0x42, 0x43));
is $bf.IO.slurp, "ABC",           '.spurt of a Buf writes the raw bytes';

# --- .mkdir creates a directory (recursively) and returns an IO::Path ---
my $sub = "$dir/a/b/c";
my $made = $sub.IO.mkdir;
ok $made ~~ IO::Path,             '.mkdir returns an IO::Path';
ok $sub.IO.d,                     '.mkdir created the directory tree';

# --- .rmdir removes an empty directory ---
my $empty = "$dir/empty";
mkdir $empty;
ok $empty.IO.d,                   'directory exists before rmdir';
is $empty.IO.rmdir, True,         '.rmdir returns True';
nok $empty.IO.e,                  '.rmdir removed the directory';

# .rmdir on a non-empty directory fails (Failure)
nok $sub.IO.parent.parent.rmdir.defined,
    '.rmdir on a non-empty directory is an undefined Failure';

# --- .unlink removes an existing file ---
my $u = "$dir/del.txt";
$u.IO.spurt("x");
ok $u.IO.e,                       'file exists before unlink';
is $u.IO.unlink, True,            '.unlink returns True for an existing file';
nok $u.IO.e,                      '.unlink removed the file';

# --- .chmod sets permission bits ---
my $c = "$dir/perm.txt";
$c.IO.spurt("p");
is $c.IO.chmod(0o644), True,      '.chmod returns True';
is $c.IO.mode, "0644",            '.chmod set the mode';
$c.IO.chmod(0o600);
is $c.IO.mode, "0600",            '.chmod can change the mode again';

# --- variable receivers (mut path / CallMethodMut) ---
my $vp = "$dir/var.txt".IO;
is $vp.spurt("vv\n"), True,       'variable receiver .spurt';
is $vp.slurp, "vv\n",             'variable receiver round-trip';
my $vd = "$dir/vardir".IO;
$vd.mkdir;
ok $vd.d,                         'variable receiver .mkdir';

# cleanup (best-effort)
$c.IO.unlink; $bf.IO.unlink; $f.IO.unlink; $vp.IO.unlink;
ok True, 'cleanup ran';
