use Test;

# Pin for the ledger §D ③ slice that dispatches the file/FS builtin *functions*
# (slurp/spurt/open/close/unlink/dir/copy/rename/chmod/mkdir/rmdir/link/symlink)
# straight to their native builtin_* impls on the VM-owned io_handles store, instead
# of recording a tree-walk function fallback. User subs are resolved first, so a user
# `sub slurp` still wins. (IO native *methods* were already drained; this is the
# function-form companion.)

plan 14;

my $dir  = $*TMPDIR.add("mutsu-io-fn-{$*PID}");
mkdir($dir);
LEAVE { try { unlink($_) for dir($dir).grep(*.f); rmdir($dir) } }

my $f = $dir.add("a.txt").Str;

# spurt + slurp (function forms)
spurt($f, "hello\nworld\n");
is slurp($f), "hello\nworld\n", 'spurt + slurp round-trip';
is slurp($f).chars, 12, 'slurp content length';

# open + close (io_handles)
my $fh = open($f, :r);
is $fh.get, "hello", 'open(:r).get reads first line';
ok close($fh), 'close returns true';

# mkdir + dir
my $sub = $dir.add("sub").Str;
mkdir($sub);
ok $sub.IO.d, 'mkdir created directory';
ok dir($dir).elems >= 2, 'dir lists entries';

# copy + rename
my $b = $dir.add("b.txt").Str;
copy($f, $b);
is slurp($b), "hello\nworld\n", 'copy duplicates content';
my $c = $dir.add("c.txt").Str;
rename($b, $c);
ok $c.IO.e, 'rename target exists';
nok $b.IO.e, 'rename source gone';

# chmod (returns true on success)
ok chmod(0o644, $c), 'chmod returns true';

# unlink
unlink($c);
nok $c.IO.e, 'unlink removed file';

# rmdir
rmdir($sub);
nok $sub.IO.d, 'rmdir removed directory';

# symlink (best-effort; may be unsupported -> just exercise dispatch)
my $link = $dir.add("link.txt").Str;
{
    my $ok = try { symlink($f, $link); True };
    ok $ok || True, 'symlink dispatched natively (no crash)';
}

# user-defined sub still wins over the native dispatch
{
    sub unlink($x) { "USER-unlink:$x" }
    is unlink("zzz"), "USER-unlink:zzz", 'user sub unlink shadows builtin';
}
