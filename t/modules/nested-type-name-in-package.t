use Test;

plan 11;

# A type whose declared name is itself nested (`class A::B`) is qualified by the
# enclosing package like any other declaration: inside `module M` it declares
# `M::A::B`. mutsu registered it under the bare `A::B`, so `M::A::B.new` could
# not find its own definition — `.^name` already said `M::A::B`, which meant the
# type object and the registry disagreed and construction died with
# "Unknown method value dispatch ... new on M::A::B".

module Outer {
    class Deep::Klass  { method who { 'klass' } }
    grammar Deep::Gram { token TOP { \d+ } }
    class Shallow      { method who { 'shallow' } }
}

is Outer::Deep::Klass.^name, 'Outer::Deep::Klass', 'a nested class name is package-qualified';
is Outer::Deep::Gram.^name, 'Outer::Deep::Gram', 'a nested grammar name is package-qualified';
is Outer::Shallow.^name,    'Outer::Shallow',    'a plain name is package-qualified as before';

is Outer::Deep::Klass.new.who, 'klass',   'a nested class can be instantiated';
is Outer::Shallow.new.who,     'shallow', 'a plain name can still be instantiated';
ok Outer::Deep::Gram.new.parse('42').defined, 'a nested grammar can be instantiated and parses';

# The redeclaration key must include the enclosing package. These two classes
# have the same written nested name, but declare distinct fully-qualified types.
module Sibling1 { class N::C { method tag { 'sibling1' } } }
module Sibling2 { class N::C { method tag { 'sibling2' } } }

is Sibling1::N::C.^name, 'Sibling1::N::C', 'first sibling package owns its nested type';
is Sibling2::N::C.^name, 'Sibling2::N::C', 'second sibling package owns its nested type';
is Sibling1::N::C.new.tag, 'sibling1', 'first sibling nested type keeps its definition';
is Sibling2::N::C.new.tag, 'sibling2', 'second sibling nested type keeps its definition';

throws-like 'module Same { class N::C {}; class N::C {} }', X::Redeclaration,
    'the same nested type still cannot be redeclared within one package';
