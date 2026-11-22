use Test;

# `.resolve` keeps a path part that does not exist as written, and a later
# `..` still drops it (rakudo's resolve walk). roast S32-io/io-path-unix.t
# test 23 resolves "/zip/loc/../../foo/bar" with a `$*CWD` of a
# non-existent "/zip/loc".

plan 10;

my $missing = '/no-such-dir-mutsu-resolve';
nok $missing.IO.e, 'the probe directory does not exist';

is "$missing/loc/../../foo/bar".IO.resolve, '/foo/bar',
    '.. drops missing parts';
is "$missing/../foo".IO.resolve, '/foo', 'one missing part, one ..';
is "/tmp/../$missing.substr(1)/x/..".IO.resolve, $missing,
    'a missing tail is popped back to its missing parent';
is "$missing/./a/../b".IO.resolve, "$missing/b", '. and .. under a missing dir';
is '/..'.IO.resolve, '/', '.. above the root stays at the root';
is '/'.IO.resolve, '/', 'the root resolves to itself';

{
    my $*CWD = "$missing/loc".IO;
    is '/foo/bar'.IO.relative.IO.absolute.IO.resolve, '/foo/bar',
        'absolute inverts relative with resolve under a missing $*CWD';
}

is "$missing/x".IO.resolve(:completely).exception.reason,
    "Doesn't resolve to an existing path",
    ':completely fails on a missing inner part, with the reason';
is '/..'.IO.resolve(:completely).exception.reason,
    'No parent dir to go back to',
    ':completely fails on .. above the root, with the reason';
