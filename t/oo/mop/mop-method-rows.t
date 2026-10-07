use Test;

# The Metamodel::*HOW metamethods are rows of the one method table
# (ADR-11276 slice 3G); each family answers as Rakudo does.

plan 21;

class P { has $.x; method m { 1 } }
class C is P { has $.y; method n { 2 } }
role R { method r { 3 } }
subset Pos of Int where * > 0;
enum E <a b>;

is C.^name, 'C', '.^name';
is C.^shortname, 'C', '.^shortname';
is-deeply C.^parents(:local).map(*.^name).List, ('P',), '.^parents(:local)';
is C.^mro.map(*.^name).head(2).List, ('C', 'P'), '.^mro';
ok C.^isa(P), '.^isa';
ok C.^can('m'), '.^can finds an inherited method';
nok C.^can('zz'), '.^can misses';
ok C.^declares_method('n'), '.^declares_method';
nok C.^declares_method('m'), '.^declares_method is local';
is C.^lookup('n').name, 'n', '.^lookup';
is C.^methods(:local).map(*.name).grep("n").List, ("n",), '.^methods(:local)';
is C.^attributes(:local).map(*.name).List, ('$!y',), '.^attributes(:local)';

class D { }
D.^add_method('hello', method { 'hi' });
D.^compose;
is D.new.hello, 'hi', '.^add_method';
D.^set_ver(v1.2);
is D.^ver, v1.2, '.^set_ver and .^ver';

is Pos.^refinee.^name, 'Int', 'SubsetHOW .^refinee';
is int32.^nativesize, 32, 'NativeHOW .^nativesize';
is int8.^unsigned, 0, 'NativeHOW .^unsigned';
is R.^candidates.elems, 1, 'ParametricRoleGroupHOW .^candidates';
is R.^pun.^name, 'R', 'ParametricRoleHOW .^pun';
throws-like { C.^candidates }, X::Method::NotFound, 'a class has no .^candidates';
throws-like { C.^no-such-metamethod }, X::Method::NotFound, 'an unknown metamethod';
is E.^enum_values.elems, 2, 'EnumHOW still answers before the rows';
