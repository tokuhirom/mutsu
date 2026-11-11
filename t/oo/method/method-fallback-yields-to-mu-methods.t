use Test;

# A class's FALLBACK sees only a name normal dispatch did not find: the
# methods every object inherits from `Mu` (`isa`, `Str`, `REPR`, `gist`) and a
# class-level attribute's accessor win over it, on the type object and on an
# instance (#16888: EEL's `Event` has a FALLBACK, and its runner asks
# `$type.^isa(Event)` of a type built by a custom metaclass).

plan 10;

class E { method FALLBACK($name, |) { "FB:$name" } }
class P is E { our $.shared = 'class-level' }

is P.isa(E), True, 'isa on the type object is Mu.isa';
is E.new.isa(E), True, 'isa on an instance';
nok P.isa(Int), 'isa answers False too';
is P.REPR, 'P6opaque', 'REPR';
like P.new.Str, /^ 'P<' /, 'Str on an instance is Mu.Str';
is P.new.gist.substr(0, 1), 'P', 'gist';
is P.shared, 'class-level', 'a class-level attribute accessor';
is P.new.frobnicate, 'FB:frobnicate', 'an unknown name still reaches FALLBACK';

class H is Metamodel::ClassHOW { }
my \T = H.new_type(:name<T>);
T.^add_parent(E);
T.^compose;
ok T.^isa(E), '.^isa through a custom metaclass';
ok T.HOW.isa(T, E), 'HOW.isa through a custom metaclass';
