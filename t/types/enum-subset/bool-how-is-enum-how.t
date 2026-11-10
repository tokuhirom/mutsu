use Test;

# `Bool` is the core enumeration: its meta-object is an `EnumHOW`.
plan 3;

is Bool.HOW.^name, 'Perl6::Metamodel::EnumHOW', 'Bool.HOW is an EnumHOW';
enum Colour <red green>;
is Colour.HOW.^name, 'Perl6::Metamodel::EnumHOW', 'a declared enum, as before';
is Int.HOW.^name, 'Perl6::Metamodel::ClassHOW', 'a class stays a ClassHOW';
