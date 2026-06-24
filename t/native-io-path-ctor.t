use Test;

# Pin for the §D ③ ctor-fork slice native-izing the `IO::Path` family `.new`
# (`IO::Path`, `IO::Path::Unix`/`::Win32`/`::Cygwin`/`::QNX`) in the VM. The
# construction is pure path-string assembly (a positional path / an IO::Path
# instance whose `path` is reused / a basename+dirname+volume triple, plus
# optional CWD/SPEC), reading only the registry — no FS, cwd, env, or user code.
# The VM builds it directly via `try_native_io_path_construct` instead of
# bouncing to the interpreter, using the single `build_io_path_instance` impl the
# interpreter's `dispatch_new` arm also delegates to (byte-identical). This is the
# constructor capstone of the IO::Path native-ization (all IO::Path methods were
# already native).

plan 19;

# --- positional path --------------------------------------------------------
my $p = IO::Path.new("foo/bar");
isa-ok $p, IO::Path, 'IO::Path.new yields an IO::Path';
is $p.Str, "foo/bar", 'positional path is stored verbatim';
is $p.basename, "bar", 'basename derived from path';
is $p.parent.Str, "foo", 'parent derived from path';

# --- basename / dirname / volume triple ------------------------------------
is IO::Path.new(:basename<file.txt>, :dirname</tmp>).Str, "/tmp/file.txt",
    'basename + dirname joined with /';
is IO::Path.new(:basename<x>).Str, "x", 'basename alone';

# --- pure lexical methods compose on the native-constructed instance --------
is IO::Path.new("base").add("child").Str, "base/child", '.add on native ctor';
is IO::Path.new("a/b/c.txt").extension, "txt", '.extension on native ctor';

# --- IO::Path instance argument reuses its path -----------------------------
my $orig = IO::Path.new("orig/path");
my $copy = IO::Path.new($orig);
isa-ok $copy, IO::Path, 'IO::Path.new(IO::Path) yields an IO::Path';
is $copy.Str, "orig/path", 'path reused from IO::Path argument';

# --- SPEC-variant subclasses ------------------------------------------------
my $w = IO::Path::Win32.new("a/b/c");
isa-ok $w, IO::Path::Win32, 'IO::Path::Win32.new yields IO::Path::Win32';
is $w.Str, "a/b/c", 'Win32 path stored';
is $w.SPEC.gist, "(Win32)", 'Win32 subclass gets IO::Spec::Win32';

my $u = IO::Path::Unix.new("x/y");
isa-ok $u, IO::Path::Unix, 'IO::Path::Unix.new yields IO::Path::Unix';
is $u.Str, "x/y", 'Unix path stored';

my $cyg = IO::Path::Cygwin.new("p/q");
isa-ok $cyg, IO::Path::Cygwin, 'IO::Path::Cygwin.new yields IO::Path::Cygwin';

# --- CWD attribute ----------------------------------------------------------
my $c = IO::Path.new("rel", :CWD</base/dir>);
is $c.Str, "rel", 'CWD does not alter the stored path';

# --- error cases ------------------------------------------------------------
dies-ok { IO::Path.new("") }, 'empty path dies';
dies-ok { IO::Path.new("a\0b") }, 'null byte in path dies';
