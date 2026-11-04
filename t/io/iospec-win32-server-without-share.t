use v6;
use Test;

# A UNC server without a share is not a volume for `splitpath`; `canonpath`,
# `catdir` and `rel2abs` read `//a` / `//a/` as the volume `\\a` and the
# separators after it as ordinary path separators (#16505).

plan 17;

my $w = IO::Spec::Win32;

is $w.splitpath('//a').join('|'), '|//|a', 'splitpath: server only';
is $w.splitpath('//a/').join('|'), '//a/||', 'splitpath: server and a separator is the whole volume';
is $w.splitpath('//a//').join('|'), '|//a//|', 'splitpath: server, empty share';
is $w.splitpath('//a//b').join('|'), '|//a//|b', 'splitpath: server, empty share, file';
is $w.splitpath('//a/b').join('|'), '//a/b||', 'splitpath: server and share keep their slashes';
is $w.splitpath('//a/b/c').join('|'), '//a/b|/|c', 'splitpath: share and file';

is $w.canonpath('//a/'), '\\\\a', 'canonpath: trailing separator after the server';
is $w.canonpath('//a//'), '\\\\a', 'canonpath: separators after the server';
is $w.canonpath('//a//b'), '\\\\a\\b', 'canonpath: empty share is squeezed';
is $w.canonpath('//a/b'), '\\\\a\\b', 'canonpath: server and share';
is $w.canonpath('\\\\a\\'), '\\\\a', 'canonpath: backslashes';

is $w.rel2abs('x', '//a/'), '\\\\a\\x', 'rel2abs: server and a separator';
is $w.rel2abs('x', '//a//b'), '\\\\a\\b\\x', 'rel2abs: empty share';
is $w.catdir('//a/', 'd'), '\\\\a\\d', 'catdir: server and a separator';
is $w.catdir('//a//', 'd'), '\\\\a\\d', 'catdir: separators after the server';
is $w.catdir('//a', 'd'), '\\\\a\\d', 'catdir: server only';
is $w.catfile('//a/', 'd'), '\\\\a\\d', 'catfile: server and a separator';
