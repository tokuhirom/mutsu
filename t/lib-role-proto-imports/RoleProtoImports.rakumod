unit module RoleProtoImports;

use RoleProtoImports::Helper :DEFAULT, :internals;

role Plain is export {
    proto method go(|operation) { my $note = plain-helper() ~ '/' ~ admit(operation); $note ~ ' ' ~ {*} }
    multi method go(Int:D $x) { "int $x" }
}

role Param[::T, Str:D $label] is export {
    proto method go(|operation) { admit(operation) ~ " $label " ~ {*} }
    multi method go(T:D $x) { "typed $x" }
}

class K1 does Plain is export { }
class K2 does Param[Int, 'k2'] is export { }
