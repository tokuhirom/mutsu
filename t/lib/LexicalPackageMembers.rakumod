# Lexical packages and classes whose members the routines below name by
# qualified name, from wherever they are called (#17720).
my class Enums { our enum Status (Ok => 0, Bad => -1); }
my package Consts { our constant Status = 5; }
my class C { our constant K = 'k'; our enum E <x y>; }
my package A { package B { our constant D = 'd'; } }
my module M { our enum Color <red green>; }

our sub enum-member is export { Enums::Status::Bad }
our sub enum-type is export { Enums::Status }
our sub package-constant is export { Consts::Status }
our sub class-constant is export { C::K }
our sub class-enum-member is export { C::E::y }
our sub nested-package-constant is export { A::B::D }
our sub module-enum-member is export { M::Color::green }
our sub enum-member-smartmatch is export { C::E::x ~~ C::E }
