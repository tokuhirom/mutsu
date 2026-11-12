use Test;

# An `ObjAt` (what `.WHICH` returns) in a string context is its identity text:
# `~`, `eq` and interpolation agree with `.Str`. Monad builds its type-cache
# keys as `M.WHICH ~ '|' ~ T.WHICH` (#17331).

plan 7;

class C { }
my $c = C.new;

is Any.WHICH ~ '|' ~ Int.WHICH, Any.WHICH.Str ~ '|' ~ Int.WHICH.Str,
    '~ of two type-object ObjAts';
is $c.WHICH ~ '', $c.WHICH.Str, '~ of an object ObjAt';
is 'x' ~ 5.WHICH, 'xInt|5', '~ of a ValueObjAt';
ok "a".WHICH eq 'Str|a', 'eq on a ValueObjAt';
is "{Any.WHICH}", Any.WHICH.Str, 'interpolation';
ok (Any.WHICH ~ '|' ~ Int.WHICH) eq (Any.WHICH ~ '|' ~ Int.WHICH),
    'the key is stable';
isnt Any.WHICH ~ '', Int.WHICH ~ '', 'two types give two keys';
