use Test;

plan 6;

# `.IO` is a Cool method; on an undefined invocant -- `Nil` or a Cool type
# object -- it is the `IO::Path` type object, not a path named "" or "(Str)".
# (#9495)

ok Nil.IO =:= IO::Path, 'Nil.IO is the IO::Path type object';
ok Str.IO =:= IO::Path, 'Str.IO too';
ok Int.IO =:= IO::Path, 'Int.IO too';
ok (Nil,)».IO[0] =:= IO::Path, 'and the hyper form';
dies-ok { Nil.IO.e }, 'a file test on it dies';
is "a".IO.basename, 'a', 'a defined Str still makes a path';
