use v6;
use Test;

# IO::Path's cwd rows (absolute, relative, CWD, raku) and its stat rows (the
# file tests and the readers of what stat returns) are rows of the built-in
# method table. A SPEC variant answers its own way, a user subclass reaches the
# same rows by its owner, and a missing path is a Failure.

plan 60;

my $dir = $*TMPDIR.add("mutsu-io-path-rows-{$*PID}");
$dir.mkdir;
LEAVE {
    for $dir.dir -> $f { $f.unlink }
    $dir.rmdir;
}

my $file = $dir.add("data.txt");
$file.spurt("hello\n");
my $empty = $dir.add("empty.txt");
$empty.spurt("");
my $missing = $dir.add("missing.txt");

# --- absolute, relative, CWD ------------------------------------------------
is "foo/bar".IO.absolute("/base"), "/base/foo/bar", 'absolute($base)';
is "/x/y".IO.absolute("/base"), "/x/y", 'an absolute path stays';
is "foo".IO.absolute, $*CWD.add("foo").Str, 'absolute is relative to $*CWD';
is "foo/bar".IO.relative("foo"), "bar", 'relative($base)';
is "/a/b/c".IO.relative("/a/x"), "../b/c", 'relative walks up with ..';
is "foo".IO.relative, "foo", 'relative to $*CWD';
is "foo".IO.CWD, $*CWD.Str, 'CWD of a plain path is $*CWD';
is IO::Path.new("foo", :CWD("/w")).CWD, "/w", 'CWD of a path made with one';
is IO::Path.new("foo", :CWD("/w")).absolute, "/w/foo", 'absolute honours the path CWD';
is IO::Path::Win32.new('C:\a\b').relative('C:\a'), 'b', 'Win32 relative';
is IO::Path::Win32.new('b').absolute('C:\a'), 'C:\a\b', 'Win32 absolute';
is IO::Path::Cygwin.new('b').absolute('/a'), '/a/b', 'Cygwin absolute';

# --- raku ----------------------------------------------------------------------
is "foo/bar".IO.raku, 'IO::Path.new("foo/bar", :SPEC(IO::Spec::Unix), :CWD("' ~ $*CWD ~ '"))',
    'raku of a path';
is-deeply "foo/bar".IO.raku.EVAL, "foo/bar".IO, 'raku round-trips';
is IO::Path::Win32.new('a\b').raku, 'IO::Path::Win32.new("a\\\\b", :CWD("' ~ $*CWD ~ '"))',
    'raku of a Win32 path omits the SPEC and escapes the backslash';

# --- file tests ----------------------------------------------------------------
ok $file.e, 'e of a file';
nok $missing.e, 'e of a missing path';
ok $file.f, 'f of a file';
nok $dir.f, 'f of a directory';
ok $dir.d, 'd of a directory';
nok $file.d, 'd of a file';
nok $file.l, 'l of a plain file';
ok $file.r, 'r';
ok $file.w, 'w';
ok $file.rw, 'rw';
nok $file.x, 'x of a data file';
nok $file.rwx, 'rwx of a data file';
ok $empty.z, 'z of an empty file';
nok $file.z, 'z of a non-empty file';
ok $dir.add("link").symlink($file) ~~ Bool | Failure, 'make a link (or fail softly)';
$dir.add("link").unlink;

# --- what stat returns ---------------------------------------------------------
is $file.s, 6, 's is the size';
is $empty.s, 0, 's of an empty file';
isa-ok $file.mode, IntStr, 'mode is an IntStr';
is $file.mode.Int +& 0o600, 0o600, 'mode has the owner bits';
ok $file.inode > 0, 'inode';
ok $file.dev >= 0, 'dev';
isa-ok $file.devtype, Int, 'devtype is an Int';
isa-ok $file.modified, Instant, 'modified is an Instant';
isa-ok $file.accessed, Instant, 'accessed is an Instant';
isa-ok $file.changed, Instant, 'changed is an Instant';
ok $file.modified <= now, 'modified is not in the future';
ok $file.created <= now, 'created is not in the future';

# --- a missing path fails ------------------------------------------------------
for <s z mode modified accessed changed inode f d r w x rw rwx> -> $m {
    my $r = $missing."$m"();
    ok $r ~~ Failure, "$m of a missing path is a Failure";
    $r.so; # handle it
}

# --- a user subclass reaches the rows by its owner --------------------------------
class MyPath is IO::Path { }
my $m = MyPath.new($file.Str);
ok $m.e, 'e on a subclass';
is $m.s, 6, 's on a subclass';
is $m.absolute, $file.absolute, 'absolute on a subclass';
is $m.raku.substr(0, 11), 'MyPath.new(', 'raku names the subclass';
