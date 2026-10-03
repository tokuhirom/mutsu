use Test;

# Native type semantics that upstream NativeCall::Types relies on
# (ADR-11203, #11204): the core native types are subtypes of their boxed
# type with an honest REPR, and the `native` declarator records `is ctype`,
# `is nativesize` and `is unsigned`.

plan 27;

# Core native types.
ok int32 ~~ Int, 'int32 ~~ Int';
ok uint8 ~~ Int, 'uint8 ~~ Int';
ok int ~~ Real, 'int ~~ Real (through Int)';
ok num32 ~~ Num, 'num32 ~~ Num';
ok str ~~ Str, 'str ~~ Str';
nok 5 ~~ int32, 'an Int value does not match a native type object';
nok uint8 ~~ int16, 'one native type object is not another, though both are Int';
my array[uint8] $na .= new(1);
nok $na ~~ array[int16], 'so array[uint8] is not array[int16]';
is int32.^mro.map(*.^name).join(','), 'int32,Int,Cool,Any,Mu', 'int32.^mro';
is num.^mro.map(*.^name).join(','), 'num,Num,Cool,Any,Mu', 'num.^mro';
is str.^mro.map(*.^name).join(','), 'str,Str,Cool,Any,Mu', 'str.^mro';
is int32.REPR, 'P6int', 'int32.REPR';
is uint.REPR, 'P6int', 'uint.REPR';
is num64.REPR, 'P6num', 'num64.REPR';
is str.REPR, 'P6str', 'str.REPR';
is Int.REPR, 'P6opaque', 'Int itself stays P6opaque';

# The `native` declarator and its traits.
my native l is Int is ctype<long> is repr<P6int> { }
my native ul is Int is ctype<long> is unsigned is repr<P6int> { }
my native sz is Int is ctype<size_t> is unsigned is repr<P6int> { }
my native fl is Num is ctype<float> is repr<P6num> { }
my native n8 is Int is nativesize(8) is repr<P6int> { }

is l.^nativesize, -4, 'is ctype<long> records the C-type code';
is l.^unsigned, 0, 'a native type is signed by default';
is ul.^unsigned, 1, 'is unsigned';
is sz.^nativesize, -6, 'is ctype<size_t>';
is fl.^nativesize, -1, 'is ctype<float> on a P6num';
is n8.^nativesize, 8, 'is nativesize(N)';
is l.REPR, 'P6int', 'a native declaration reports its is repr';
is fl.REPR, 'P6num', 'a P6num native declaration';
ok ul ~~ Int, 'a native declaration is a subtype of its parent';

# On a class these traits fail as rakudo's do: only NativeHOW has the setters.
throws-like { EVAL 'class NotNative1 is unsigned { }' }, Exception,
    message => /'set_unsigned'/, 'is unsigned on a class';
throws-like { EVAL 'class NotNative2 is ctype<long> { }' }, Exception,
    message => /'set_ctype'/, 'is ctype on a class';
