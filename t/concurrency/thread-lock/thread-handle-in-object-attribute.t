use Test;

# A file handle an object keeps in an attribute is usable from a worker
# thread. A spawned thread is handed clones of the handles its env references,
# and only handles bound directly to a variable were found, so a worker's
# `$!h.print` died "Invalid IO::Handle". Reduced from Log::Dispatch::File,
# which keeps `has IO::Handle $!log-h` and writes from a `start react`.

plan 3;

my $path = $*TMPDIR.add("mutsu-thread-handle-attr-$*PID.txt");
LEAVE { $path.unlink if $path.e }
$path.unlink if $path.e;

class W {
    has $.file;
    has IO::Handle $!h;
    submethod TWEAK { $!h = $!file.open(:a) }
    method w($s) { $!h.print($s) }
    method close { $!h.close }
}
class Holder { has $.w }

my $w = W.new(file => $path);
await start { $w.w("a\n") };
await start { $w.w("b\n") };
my $holder = Holder.new(w => $w);
await start { $holder.w.w("c\n") };
lives-ok { $w.w("d\n") }, 'the parent can still write';
$w.close;
is $path.slurp, "a\nb\nc\nd\n", 'writes from worker threads reach the file';
ok $path.e, 'file exists';
