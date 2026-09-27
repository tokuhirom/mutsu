use Test;

# Two packages declaring an enum with the same short name get two distinct
# enum types (#9654). Rakudo keeps displaying each under its declared short
# name (`.^name`, `.raku`, gist), which is only a display quirk: identity
# follows the declaring package.

plan 21;

module A {
    our enum pn <x y>;
    our sub keys-here { pn.enums.keys.sort }
    our sub key-of(pn $v) { $v.key }
    our sub bare-name { x.^name }
    our sub coerce($i) { pn($i) }
}
module B {
    our enum pn <z>;
    our sub keys-here { pn.enums.keys.sort }
}

# Identity: each package's routines see their own enum, whatever was declared last.
is-deeply A::keys-here(), ('x', 'y').Seq, 'first package keeps its own enum';
is-deeply B::keys-here(), ('z',).Seq, 'second package has its own enum';
is-deeply A::pn.enums.keys.sort, ('x', 'y').Seq, 'qualified spelling of the first enum';
is-deeply B::pn.enums.keys.sort, ('z',).Seq, 'qualified spelling of the second enum';
ok A::x ~~ A::pn, 'a value matches its own enum type';
nok B::z ~~ A::pn, "a value does not match the other package's same-named enum";
nok A::x === B::z, 'values of the two enums are not identical';
is A::key-of(A::y), 'y', 'enum type constraint inside the package';
is A::coerce(1).raku, 'pn::y', 'coercion call resolves the enum of the package';

# Display: the declared short name, as rakudo prints it.
is A::pn.^name, 'pn', '.^name of the type object';
is A::x.^name, 'pn', '.^name of a value';
is A::bare-name(), 'pn', '.^name of a bare value inside the package';
is A::x.raku, 'pn::x', '.raku of a value';
is A::pn.raku, 'pn', '.raku of the type object';
is A::pn.gist, '(pn)', 'gist of the type object';
is A::x.WHAT.gist, '(pn)', 'gist of a value .WHAT';

my A::pn $v = A::y;
is $v, 'y', 'typed variable with the qualified enum type';
throws-like { my A::pn $w = B::z }, X::TypeCheck::Assignment,
    message => /'expected pn but got pn'/, 'assignment type check names the short name';

# An enum in a class body is a nested type the class's methods reach.
class C {
    enum E <p q>;
    method type-name { E.^name }
    method value-raku { q.raku }
}
is C.type-name, 'E', 'class-body enum resolves inside a method';
is C.value-raku, 'E::q', 'class-body enum value renders under the short name';

# An exported package-scoped enum type is imported by its short name.
module M { enum Color is export <red green> }
import M;
is Color.enums.keys.sort.join(','), 'green,red', 'imported enum type by short name';
