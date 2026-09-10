use v6;
use Test;

# `$*VM.platform-library-name` decorates only the BASENAME and puts the
# directory back afterwards. Decorating the whole string produced the nonsense
# `lib/bar/foo.so` for `/bar/foo`, so a binding that loads a `.so` it built next
# to itself (`NativeLibs::Compile`) got an unopenable path. A name that carries
# any directory is additionally made absolute.
#
# (Deciding whether a name is *already* decorated is the caller's job — see
# `NativeLibs::cannon-name`, which checks `.extension` first.)

plan 5;

sub pl($p, |c) { $*VM.platform-library-name($p.IO, |c).Str }

my $win = $*DISTRO.is-win;
my $ext = $win ?? '.dll' !! ($*VM.osname eq 'darwin' ?? '.dylib' !! '.so');
my $pre = $win ?? '' !! 'lib';

is pl('foo'), "{$pre}foo$ext", 'a bare stem stays relative and is decorated';
is pl('/bar/foo'), "/bar/{$pre}foo$ext", 'an absolute path decorates only the basename';
is pl('./foo'), $*CWD.add("{$pre}foo$ext").Str, 'a CWD-relative path is made absolute';
is pl('a/b/foo'), $*CWD.add("a/b/{$pre}foo$ext").Str,
   'a nested relative path keeps its directory and is made absolute';

# `:version` names an ABI-versioned library: after the extension on Linux,
# before it on macOS, and not at all on Windows.
my $want = $win               ?? "foo$ext"
        !! $*VM.osname eq 'darwin' ?? "libfoo.2$ext"
        !!                           "libfoo{$ext}.2";
is pl('foo', :version(Version.new(2))), $want, 'the ABI version is placed per platform';

# vim: expandtab shiftwidth=4
