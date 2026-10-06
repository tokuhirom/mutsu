use v6;
use Test;

# The path-syntax methods of IO::Spec::Unix, ::Win32, ::Cygwin and ::QNX are
# rows of the built-in method table. Rakudo declares each on the classes that
# override it, and the others inherit it from IO::Spec::Unix; one handler serves
# every class and reads which one the receiver is.

plan 80;

# --- Unix --------------------------------------------------------------------
my \U = IO::Spec::Unix;
is U.canonpath('/a//b/./c/../d/'), '/a/b/c/../d', 'canonpath folds separators and .';
is U.canonpath('a/b/..', :parent), 'a', 'canonpath :parent folds ..';
is U.catdir('a', 'b', 'c'), 'a/b/c', 'catdir';
is U.catdir(<x y>), 'x/y', 'catdir flattens a list';
is U.catdir('/', 'a'), '/a', 'catdir of the root';
is U.catdir(), '', 'catdir of nothing';
is U.catdir(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), '1/2/3/4/5/6/7/8/9/10', 'catdir takes any number of parts';
is U.catfile('a', 'b', 'c.txt'), 'a/b/c.txt', 'catfile';
is U.catpath('', '/dir', 'file'), '/dir/file', 'catpath';
is U.join('', '/dir', 'file'), '/dir/file', 'join';
is U.join('', 'a', 'b'), 'a/b', 'join of relative parts';
is U.split('/a/b/c.txt').basename, 'c.txt', 'split basename';
is U.split('/a/b/c.txt').dirname, '/a/b', 'split dirname';
is U.split('/a/b/c.txt').^name, 'IO::Path::Parts', 'split is an IO::Path::Parts';
is-deeply U.splitpath('/a/b/c.txt'), ("", "/a/b/", "c.txt"), 'splitpath';
is-deeply U.splitpath('/a/b/', :nofile), ("", "/a/b/", ""), 'splitpath :nofile';
is-deeply U.splitdir('/a/b/c'), ("", "a", "b", "c"), 'splitdir';
ok U.is-absolute('/a'), 'is-absolute';
nok U.is-absolute('a'), 'a relative path is not absolute';
is U.abs2rel('/a/b/c', '/a/x'), '../b/c', 'abs2rel';
is U.rel2abs('b/c', '/a'), '/a/b/c', 'rel2abs';
is U.basename('/a/b/c.txt'), 'c.txt', 'basename';
is U.extension('/a/b/c.txt'), 'txt', 'extension';
is U.curdir, '.', 'curdir';
is U.updir, '..', 'updir';
is U.rootdir, '/', 'rootdir';
is U.dir-sep, '/', 'dir-sep';
is U.devnull, '/dev/null', 'devnull';
isa-ok U.tmpdir, IO::Path, 'tmpdir is an IO::Path';
isa-ok U.path, Seq, 'path is a Seq';

# --- Win32 -------------------------------------------------------------------
my \W = IO::Spec::Win32;
is W.canonpath('C:\a\.\b\\c'), 'C:\a\b\c', 'Win32 canonpath';
is W.canonpath('C:/a/b'), 'C:\a\b', 'Win32 canonpath turns / into \\';
is W.catdir('a', 'b', 'c'), 'a\b\c', 'Win32 catdir';
is W.catdir(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), '1\2\3\4\5\6\7\8\9\10', 'Win32 catdir takes any number of parts';
is W.catfile('a', 'b', 'c.txt'), 'a\b\c.txt', 'catfile is inherited and follows the class';
is W.catpath('C:', '/dir', 'file'), 'C:/dir\file', 'Win32 catpath';
is W.join('C:', '/dir', 'file'), 'C:/dir\file', 'Win32 join';
is W.split('C:\a\b.txt').volume, 'C:', 'Win32 split volume';
is W.split('C:\a\b.txt').basename, 'b.txt', 'Win32 split basename';
is-deeply W.splitpath('C:\a\b.txt'), ("C:", '\a\\', "b.txt"), 'Win32 splitpath';
is-deeply W.splitdir('a\b/c'), ("a", "b", "c"), 'Win32 splitdir splits on both separators';
ok W.is-absolute('C:\a'), 'Win32 is-absolute';
nok W.is-absolute('a\b'), 'a relative Win32 path';
is W.abs2rel('C:\a\b\c', 'C:\a\x'), '..\b\c', 'abs2rel is inherited and follows the class';
is W.rel2abs('b', 'C:\a'), 'C:\a\b', 'Win32 rel2abs';
is W.basename('C:\a\b.txt'), 'b.txt', 'Win32 basename';
is W.curdir, '.', 'curdir is inherited';
is W.rootdir, '\\', 'Win32 rootdir';
is W.dir-sep, '\\', 'Win32 dir-sep';
is W.devnull, 'nul', 'Win32 devnull';
isa-ok W.tmpdir, IO::Path, 'Win32 tmpdir';

# --- Cygwin ------------------------------------------------------------------
my \C = IO::Spec::Cygwin;
is C.canonpath('/a//b/./c'), '/a/b/c', 'Cygwin canonpath';
is C.canonpath('C:\a\.\b'), 'C:/a/b', 'Cygwin canonpath turns \\ into /';
is C.catdir('a', 'b'), 'a/b', 'Cygwin catdir';
is C.catpath('C:', '/dir', 'file'), 'C:/dir/file', 'Cygwin catpath';
is C.join('C:', '/dir', 'file'), 'C:/dir/file', 'Cygwin join';
ok C.is-absolute('C:\a'), 'Cygwin is-absolute of a drive path';
is C.abs2rel('/a/b/c', '/a/x'), '../b/c', 'Cygwin abs2rel';
is C.rel2abs('b', '/a'), '/a/b', 'Cygwin rel2abs';
is C.rootdir, '/', 'rootdir is inherited';
is C.curdir, '.', 'curdir is inherited';
is-deeply C.splitpath('/a/b.txt'), ("", "/a/", "b.txt"), 'Cygwin splitpath';

# --- QNX ---------------------------------------------------------------------
my \Q = IO::Spec::QNX;
is Q.canonpath('//a/b'), '//a/b', 'QNX canonpath keeps a leading //';
is Q.canonpath('/a//b'), '/a/b', 'QNX canonpath folds the rest';
is Q.catfile('a', 'b'), 'a/b', 'catfile is inherited';
is Q.rootdir, '/', 'rootdir is inherited';
is Q.basename('/a/b'), 'b', 'basename is inherited';
is Q.dir-sep, '/', 'dir-sep is inherited';

# --- $*SPEC and instances -----------------------------------------------------
is $*SPEC.catdir('x', 'y'), 'x/y', '$*SPEC is a row receiver';
is $*SPEC.^name, 'IO::Spec::Unix', '$*SPEC on this platform';
is IO::Spec::Unix.new.catdir('a', 'b'), 'a/b', 'an instance answers too';
is $*SPEC.catfile($*SPEC.rootdir, 'x'), '/x', 'methods compose';
is $*SPEC.join('', 'a', 'b'), 'a/b', 'join through $*SPEC';
is $*SPEC.splitdir('a/b').join('+'), 'a+b', 'splitdir through $*SPEC';
is-deeply $*SPEC.canonpath("a//b", :parent), "a/b", 'canonpath with a named argument';
is IO::Path.new("a//b/./c").cleanup.Str, "a/b/c", 'IO::Path.cleanup is not an IO::Spec row';
throws-like { U.no-such-method }, X::Method::NotFound, 'an unknown method is not found';
is U.canonpath("x", :nosuch), 'x', 'an undeclared named argument is ignored';
is-deeply U.splitdir("a/b"), ("a", "b"), 'splitdir of a relative path';
is U.catfile("a", "b").IO.basename, 'b', 'a row answer is an ordinary string';
