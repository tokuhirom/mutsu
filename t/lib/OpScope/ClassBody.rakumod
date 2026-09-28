# A `use` inside a class body is lexical to that body.
unit module OpScope::ClassBody;
class K { use OpScope::Pow; method m { 2 ** 3 } }
sub plain-pow is export { 2 ** 3 }
sub class-pow is export { K.m }
