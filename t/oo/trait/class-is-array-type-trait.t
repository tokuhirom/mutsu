use v6;
use Test;

plan 7;

# `is array_type(T)` is a core trait: `trait_mod:<is>(Mu:U, :$array_type!)`
# calls `.^set_array_type`, and a role's trait applies to each class that
# composes it, with the role's type arguments bound (#11310). Upstream
# `NativeCall::Types` declares its typed arrays this way.

class Ints is repr('CArray') is array_type(int32) { }
is Ints.^array_type.^name, 'int32', 'a class records its own array_type';

class Plain { }
is Plain.^array_type.^name, 'Mu', 'a class without one answers Mu';

role Typed[::T] is array_type(T) { }
class Nums is repr('CArray') does Typed[num64] { }
is Nums.^array_type.^name, 'num64', 'a parametric role applies its trait with T bound';

role Fixed is array_type(int16) { }
class Shorts is repr('CArray') does Fixed { }
is Shorts.^array_type.^name, 'int16', 'a plain role applies its trait to the class';

class Own is repr('CArray') is array_type(int8) does Typed[int32] { }
is Own.^array_type.^name, 'int8', "the class's own trait wins over a role's";

class Later { }
Later.^set_array_type(uint8);
is Later.^array_type.^name, 'uint8', '.^set_array_type records it';

# A user candidate in scope does not capture the core trait.
multi trait_mod:<is>(Routine $r, :$unrelated!) { }
class Still is repr('CArray') is array_type(int64) { }
is Still.^array_type.^name, 'int64', 'a user trait_mod:<is> does not capture array_type';
