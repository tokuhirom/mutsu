use v6;
use Test;

# The IO::Path methods that derive a path (or a string, or a bool) from the
# receiver's own attributes -- no filesystem, no cwd -- are rows of the built-in
# method table. Every SPEC variant shares them and keeps its own class in the
# answer; a user subclass reaches the same rows through its owner.

plan 56;

my $p = "foo/bar/baz.tar.gz".IO;

# --- the parts of a path -----------------------------------------------------
is $p.basename, "baz.tar.gz", 'basename';
is $p.dirname, "foo/bar", 'dirname';
is $p.volume, "", 'volume of a relative Unix path is empty';
is $p.Str, "foo/bar/baz.tar.gz", 'Str is the path as given';
is $p.gist, '"foo/bar/baz.tar.gz".IO', 'gist is the .IO call';
isa-ok $p.IO, IO::Path, 'IO is an IO::Path';
is $p.IO.Str, $p.Str, 'IO is the same path';
is $p.SPEC.^name, "IO::Spec::Unix", 'SPEC is the Unix IO::Spec';

my $parts = $p.parts;
is $parts.^name, "IO::Path::Parts", 'parts is an IO::Path::Parts';
is-deeply ($parts.volume, $parts.dirname, $parts.basename),
    ("", "foo/bar", "baz.tar.gz"), 'parts reads volume, dirname and basename';

# --- extension ---------------------------------------------------------------
is $p.extension, "gz", 'extension is the last part';
is $p.extension(:parts(2)), "tar.gz", 'extension :parts(2)';
is $p.extension(:parts(0..2)), "tar.gz", 'extension :parts(0..2) takes the longest';
is $p.extension("zip").Str, "foo/bar/baz.tar.zip", 'extension replaces';
is $p.extension("x", :parts(2), :joiner("_")).Str, "foo/bar/baz_x",
    'extension takes :parts and :joiner';
is $p.extension("").Str, "foo/bar/baz.tar", 'extension("") removes the extension';

# --- parent, sibling, add, child ------------------------------------------------
is $p.parent.Str, "foo/bar", 'parent';
is $p.parent(2).Str, "foo", 'parent(2)';
is $p.parent(0).Str, $p.Str, 'parent(0) is the path itself';
is ".".IO.parent.Str, "..", 'the parent of . is ..';
is "..".IO.parent.Str, "../..", 'the parent of .. stacks another ..';
throws-like { $p.parent(-1) }, Exception, 'a negative parent is out of range';
is $p.sibling("q").Str, "foo/bar/q", 'sibling';
is $p.add("x").Str, "foo/bar/baz.tar.gz/x", 'add one child';
is $p.add("x", "y").Str, "foo/bar/baz.tar.gz/x/y", 'add several children';
is $p.add(<a b>).Str, "foo/bar/baz.tar.gz/a/b", 'add flattens a list of children';
is $p.add(1, 2, 3, 4, 5, 6, 7, 8, 9).Str, "foo/bar/baz.tar.gz/1/2/3/4/5/6/7/8/9",
    'add takes any number of children';
is $p.child("c d").Str, "foo/bar/baz.tar.gz/c d", 'child keeps its name whole';
is "/".IO.add("x").Str, "/x", 'adding to the root';
is "/tmp/x".IO.sibling("y", :zzz).Str, "/tmp/y", 'an undeclared named is not an argument';

# --- cleanup, is-absolute, succ, pred ---------------------------------------------
is "a/./b/../c".IO.cleanup.Str, "a/b/../c", 'cleanup folds . but not ..';
is "a//b/".IO.cleanup.Str, "a/b", 'cleanup folds repeated and trailing separators';
ok "/x".IO.is-absolute, 'is-absolute';
nok "x".IO.is-absolute, 'a relative path is not absolute';
ok "x".IO.is-relative, 'is-relative';
nok "/x".IO.is-relative, 'an absolute path is not relative';
is "a9".IO.succ.Str, "b0", 'succ steps the basename';
is "d/a9".IO.pred.Str, "d/a8", 'pred steps the basename';

# --- the SPEC variants keep their class -------------------------------------------
my $w = IO::Path::Win32.new('C:\foo\bar');
is $w.volume, "C:", 'Win32 volume';
is $w.basename, "bar", 'Win32 basename';
is $w.parent.^name, "IO::Path::Win32", 'the parent of a Win32 path is a Win32 path';
is $w.parent.Str, 'C:\foo', 'Win32 parent';
is $w.add("x").^name, "IO::Path::Win32", 'add on a Win32 path';
ok $w.is-absolute, 'a Win32 path with a volume is absolute';
my $u = IO::Path::Unix.new("a/b");
is $u.sibling("c").^name, "IO::Path::Unix", 'sibling on a Unix path';
is IO::Path::Cygwin.new("a/b").parent.^name, "IO::Path::Cygwin", 'Cygwin parent';
is IO::Path::QNX.new("a/b").parent.^name, "IO::Path::QNX", 'QNX parent';

# --- user subclasses --------------------------------------------------------------
class MyPath is IO::Path { }
my $m = MyPath.new("x/y/z");
is $m.basename, "z", 'a subclass answers basename';
is $m.parent.^name, "MyPath", 'the parent of a subclass is the subclass';
is $m.parent.Str, "x/y", 'the subclass parent path';
is $m.add("q").^name, "MyPath", 'add on a subclass keeps the class';
is $m.child("k").Str, "x/y/z/k", 'child on a subclass';

class OverridePath is IO::Path { method basename { "override" } }
is OverridePath.new("x/y").basename, "override", 'a subclass method wins over the row';
is OverridePath.new("x/y").dirname, "x", 'a method the subclass does not override still answers';

# --- Cool methods still see the path ----------------------------------------------
ok "foo/bar".IO.starts-with("foo/"), 'starts-with stringifies the path';
is "abc".IO.uc, "ABC", 'uc stringifies the path';
