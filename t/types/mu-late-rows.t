use Test;

# `Mu`'s and `Cool`'s last-resort methods are late rows, found by the
# receiver's MRO after every other answer declined (ADR-11276 §9.138).
# Answers checked against Rakudo.

plan 17;

class Foo { }
role R { }
subset Pos of Int where * > 0;
class S { method Str { 'sss' } }

is Foo.gist, '(Foo)', 'gist of a type object';
is Foo.raku, 'Foo', 'raku of a type object';
like Foo.new.Str, /Foo/, 'Str of an instance';
is Foo.REPR, 'P6opaque', 'REPR of a type object';
is Foo.new.REPR, 'P6opaque', 'REPR of an instance';
is Int:D.REPR, 'Uninstantiable', 'REPR of a definite type';
ok Pos.isa(Int), 'a subset isa its base';
ok R.isa(Any), 'a role isa Any';
nok R.isa(Foo), 'a role is not another class';
ok Foo.isa(Any), 'a class isa Any';
ok Foo.isa('Any'), 'isa by name';
is any(1, 2).raku, 'any(1, 2)', 'raku of a junction';
is "1+2".EVAL, 3, 'EVAL of a Str';
dies-ok { Blob.new(49).EVAL }, 'a Blob is not Cool and has no EVAL';
is S.new.Stringy, 'sss', 'Stringy calls a user Str';
is S.Stringy, 'sss', 'Stringy of a type object calls a user Str';
throws-like { Foo.new.Numeric }, X::Multi::NoMatch, 'Numeric of an instance';
