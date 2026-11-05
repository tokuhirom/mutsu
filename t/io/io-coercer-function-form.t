use v6;
use Test;

# `IO($x)` is the coercer form of `$x.IO` (POSIX::PWDENT's t/00 uses it).

plan 3;

ok IO('a/b') ~~ IO::Path, 'IO(Str) is an IO::Path';
is IO('a/b').basename, 'b', 'with the path of the string';
ok IO('a/b'.IO) ~~ IO::Path, 'IO(IO::Path) too';
