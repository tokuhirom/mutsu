use Test;

# From the TAP distribution: an `enum` written inside a method is that
# routine's lexical. Its type name resolves in a typed declaration, and its
# keys shadow a same-named key of an enum declared later at the top level.

plan 5;

class A {
    method m {
        enum Mode <Normal SubTest Yaml>;
        my Mode $mode = Normal;
        return $mode;
    }
    method n {
        enum Mode2 <Normal Other>;
        return Normal === Mode2::Normal;
    }
}

enum Volume (:Silent(-2) :Normal(0) :Verbose(1));

is Normal.Int, 0, 'the top-level key is the top-level one before the routine runs';
is Normal.^name, 'Volume', 'and belongs to its own enum';
is A.m.Str, 'Normal', 'typed my of a method-local enum';
ok A.n, 'method-local key shadows a later top-level key of the same name';
is A.m.^name, 'Mode', 'the enum keeps its own name';
