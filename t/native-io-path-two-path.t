use Test;

# Pin for VM-native two-path `IO::Path` filesystem operations (ledger §D):
# `.copy` / `.rename` / `.move` / `.symlink` / `.link`. Each resolves both the
# receiver path and the destination/link-name path against the VM-owned cwd and
# performs a one-shot syscall (`fs::copy`/`fs::rename`/`unix_fs::symlink`/
# `fs::hard_link`), allocating no io_handles, so the VM dispatches them natively
# via `try_io_path_two_path_op` — the single impl `native_io_path` also delegates
# to. Both literal and variable (`$p.copy(...)` -> CallMethodMut) receivers are
# exercised. The directory is recreated fresh so stale files can't perturb the
# create-new / exclusive checks.

plan 18;

my $dir = "tmp/native-io-twopath-$*PID";
# Start from a clean directory.
$dir.IO.mkdir;
.unlink for $dir.IO.dir;

# --- .copy ---
my $src = "$dir/src.txt";
spurt $src, "payload\n";
my $dst = "$dir/dst.txt";
is $src.IO.copy($dst), True,        '.copy returns True';
is $dst.IO.slurp, "payload\n",      '.copy duplicated the content';
ok $src.IO.e,                       '.copy left the source in place';

# .copy onto itself is a Failure (not an exception)
nok $src.IO.copy($src).defined,     '.copy onto itself is an undefined Failure';

# .copy(:createonly) onto an existing file is a Failure
nok $src.IO.copy($dst, :createonly).defined,
    '.copy(:createonly) onto an existing file is an undefined Failure';

# --- .rename ---
my $r1 = "$dir/r1.txt";
spurt $r1, "rdata\n";
my $r2 = "$dir/r2.txt";
is $r1.IO.rename($r2), True,        '.rename returns True';
nok $r1.IO.e,                       '.rename removed the old name';
is $r2.IO.slurp, "rdata\n",         '.rename preserved the content';

# --- .move (alias-ish of rename) ---
my $m1 = "$dir/m1.txt";
spurt $m1, "mdata\n";
my $m2 = "$dir/m2.txt";
is $m1.IO.move($m2), True,          '.move returns True';
nok $m1.IO.e,                       '.move removed the source';
is $m2.IO.slurp, "mdata\n",         '.move preserved the content';

# --- .symlink ---
my $tgt = "$dir/target.txt";
spurt $tgt, "linked\n";
my $sym = "$dir/sym.txt";
is $tgt.IO.symlink($sym), True,     '.symlink returns True';
is $sym.IO.slurp, "linked\n",       '.symlink resolves to the target content';

# --- .link (hard link) ---
my $ht = "$dir/hard-target.txt";
spurt $ht, "hard\n";
my $hl = "$dir/hard-link.txt";
is $ht.IO.link($hl), True,          '.link returns True';
is $hl.IO.slurp, "hard\n",          '.link sees the same content';

# --- variable receiver (mut path / CallMethodMut) ---
my $vs = "$dir/vsrc.txt";
spurt $vs, "vv\n";
my $vp = $vs.IO;
is $vp.copy("$dir/vdst.txt"), True, 'variable receiver .copy';
is "$dir/vdst.txt".IO.slurp, "vv\n", 'variable receiver .copy content';

# cleanup
.unlink for $dir.IO.dir;
$dir.IO.rmdir;
ok True, 'cleanup ran';
