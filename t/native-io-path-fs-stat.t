use Test;

# Pin for VM-native `IO::Path` filesystem `stat`-only file tests / accessors
# (ledger §D): `.e`/`.f`/`.d`/`.l`/`.r`/`.w`/`.x`/`.rw`/`.rwx`/`.z`/`.s`/`.mode`/
# `.created`/`.modified`/`.accessed`/`.changed`. These resolve the receiver's path
# against the VM-owned cwd and read the filesystem via `stat` only (no io_handles
# allocation, no content read), so the VM dispatches them natively via
# `try_io_path_fs_stat` — the single impl `native_io_path` also delegates to.
# Both literal (`"x".IO.e`, non-mut path) and variable (`$p.e` -> CallMethodMut,
# mut path) receivers are exercised. Behavior is byte-identical to the interpreter.

plan 30;

# Build a known file + directory under the gitignored tmp/ tree.
my $dir = "tmp/native-io-fs-stat-$*PID";
mkdir $dir;
my $file = "$dir/data.txt";
spurt $file, "hello\n";          # 6 bytes
my $empty = "$dir/empty.txt";
spurt $empty, "";
my $missing = "$dir/does-not-exist.txt";

# --- literal-ish receivers (the .IO coercion + method) ---
ok  $file.IO.e,           '.e true for an existing file';
ok  $file.IO.f,           '.f true for a regular file';
nok $file.IO.d,           '.d false for a regular file';
ok  $dir.IO.d,            '.d true for a directory';
nok $dir.IO.f,            '.f false for a directory';
is  $file.IO.s, 6,        '.s is the byte size';
nok $file.IO.z,           '.z false for a non-empty file';
ok  $empty.IO.z,          '.z true for an empty file';
is  $empty.IO.s, 0,       '.s is 0 for an empty file';
ok  $file.IO.r,           '.r true (readable)';
ok  $file.IO.w,           '.w true (writable)';

# --- missing path: predicates return False / Failure (not exceptions) ---
nok $missing.IO.e,        '.e false for a missing path';
nok $missing.IO.f.defined, '.f on a missing path is an undefined Failure';
nok $missing.IO.d.defined, '.d on a missing path is an undefined Failure';

# --- variable receiver: hits the mut path (CallMethodMut) ---
my $p = $file.IO;
ok  $p.e,                 'variable receiver .e';
ok  $p.f,                 'variable receiver .f';
is  $p.s, 6,              'variable receiver .s';
nok $p.z,                 'variable receiver .z';
my $pd = $dir.IO;
ok  $pd.d,                'variable receiver .d on a directory';
my $pm = $missing.IO;
nok $pm.e,                'variable receiver .e on a missing path';

# --- stat accessors are positive timestamps within the file's lifetime ---
# (mutsu returns Int epoch seconds; raku returns an Instant. Both are defined and
#  numify > 0, so the pin only asserts that — not the concrete type.)
ok  $file.IO.modified.defined,  '.modified is defined';
ok  $file.IO.modified > 0,      '.modified is a positive epoch';
ok  $file.IO.accessed > 0,      '.accessed is a positive epoch';
ok  $file.IO.changed  > 0,      '.changed is a positive epoch';

# .mode is a Str of octal permission bits
ok  $file.IO.mode ~~ Str,       '.mode is a Str';
like $file.IO.mode, /^ \d ** 3..4 $/, '.mode looks like octal digits';

# --- consistency: a missing file's .s throws (X::IO::DoesNotExist family) ---
dies-ok { $missing.IO.s }, '.s on a missing path dies/fails';

# --- receiver is not mutated by a stat call (no writeback) ---
my $orig = $file.IO;
$orig.e;
is $orig.Str, $file, 'stat does not mutate the receiver path';

# --- the sub forms still work (route through the same dispatch) ---
ok  $file.IO.e && $dir.IO.d, 'combined file + dir tests';

# cleanup
unlink $file;
unlink $empty;
rmdir $dir;
ok True, 'cleanup ran';
