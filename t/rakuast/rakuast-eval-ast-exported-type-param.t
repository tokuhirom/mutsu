use v6;
use Test;

# A unit that `use`s a module and then names one of its exported types in a
# signature, or a routine it exports as a bareword, is judged against what the
# `use` really imported. A type the module declares without `is export` stays
# unknown. This passes under both mutsu and raku.

plan 4;

is EVAL(q[use NativeCall; sub f(Pointer $p) { 1 }; f(Pointer)]), 1,
    'an exported NativeCall type in a parameter';
is EVAL(q[use NativeCall; my array[uint8] $a .= new(1, 2, 3); $a.elems]), 3,
    'array[T] as an invocant is a type';
is EVAL(q[use Telemetry; T<wallclock> > 0]), True,
    'an exported routine as a bareword before a subscript';
throws-like q[use NativeCall; sub f(NoSuchTypeAnywhere $p) { }; 1], X::Parameter::InvalidType,
    'a type nothing exports is still invalid';
