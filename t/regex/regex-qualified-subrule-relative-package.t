use Test;

plan 4;

# A `::`-qualified subrule reference is resolved relative to the package it is
# written in: `<Schema::Core::element>` inside `module YAMLish` names
# `YAMLish::Schema::Core::element` (which is how YAMLish's `to-yaml` decides
# whether a string needs quoting).
module M {
    grammar S::C {
        token num { \d+ }
        proto token el { * }
        token el:<a> { 'a' }
    }
    our sub relative-neg($s) { so $s ~~ / ^ <!S::C::el> \w+ $ / }
    our sub relative-pos($s) { so $s ~~ / ^ <S::C::num> $ / }
    our sub absolute-neg($s) { so $s ~~ / ^ <!M::S::C::el> \w+ $ / }
}

ok M::relative-neg('bcd'), 'a package-relative <!Pkg::rule> passes when the rule does not match';
nok M::relative-neg('abc'), 'and fails when it does';
ok M::relative-pos('123'), 'a package-relative <Pkg::rule> matches';
ok M::absolute-neg('bcd'), 'the fully qualified spelling still works';
