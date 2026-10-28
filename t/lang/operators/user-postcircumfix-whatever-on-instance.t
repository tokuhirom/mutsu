use Test;

plan 6;

# A user `postcircumfix:<[ ]>` candidate taking `Whatever` is consulted for an
# instance (and for a punned role instance), as one taking `Int` already is
# (Dan's `DataFrame:D $df, Whatever`).
class Foo does Positional {
    has @.d = 1, 2, 3;
    method AT-POS($i) { @!d[$i] }
}
role Fr does Positional {
    has @.d = 1, 2, 3;
    method AT-POS($i) { @!d[$i] }
}
class Via does Fr { }

multi postcircumfix:<[ ]>(Foo:D $f, Int $p) { "int" }
multi postcircumfix:<[ ]>(Foo:D $f, Whatever) { "whatever" }
multi postcircumfix:<[ ]>(Foo:D $f, WhateverCode $p) { "whatever-code" }
multi postcircumfix:<[ ]>(Fr:D $f, Whatever) { "role-whatever" }

my \f = Foo.new;
is f[0], 'int', 'Int subscript reaches the user candidate';
is f[*], 'whatever', 'Whatever subscript reaches the user candidate';
is f[*-1], 'whatever-code', 'WhateverCode subscript reaches the user candidate';
is Via.new[*], 'role-whatever', 'a class composing the role';
is Fr.new[*], 'role-whatever', 'a punned role instance';

my @plain = 1, 2, 3;
is @plain[*].elems, 3, 'a plain Array still slices';
