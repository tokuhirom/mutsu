use Test;

# The keys of an enum declared in a class or grammar body are scoped to that
# body, like the enum type itself; they do not leak as bare terms into the
# enclosing scope. PDF::Grammar declares `enum AST-Types <array ...>` in its
# grammar body, and the leaked key made every later `array[uint64]` in the
# program index the enum value instead of naming the native array type.

plan 7;

grammar G {
    enum E is export(:E) <array hash>;
    method key { array }
}

class C {
    enum F <foo bar>;
    method both { foo, bar }
    method qualified { F::foo }
}

is array[uint64].new(3).^name, 'array[uint64]', 'array is still the native array type';
is G::array, 'array', 'the key is reachable package-qualified';
is G.key, 'array', 'and as a bare term inside the body';
is-deeply C.both.map(*.key).List, <foo bar>, "a class's methods see its enum keys";
is C.qualified.key, 'foo', 'and through the enum type';
is C::foo.key, 'foo', 'outside, through the package';
throws-like 'foo', X::Undeclared::Symbols, 'but not as a bare term outside';
