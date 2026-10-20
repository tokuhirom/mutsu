use Test;

plan 5;

# A qualified class name declared inside a package is relative to that
# package: `class M::C` inside `module M` has identity `M::M::C`, while its
# written binding `M::C` points to that identity (Rakudo 2026.07).

module MyModule {
    class MyClass { method value { 11 } }
    class MyModule::MyClass { method value { 22 } }
}

is MyModule::MyClass.^name, 'MyModule::MyModule::MyClass',
    'written binding names the relative declaration identity';
is MyModule::MyClass.new.value, 22, 'the binding constructs the later declared class';
throws-like { EVAL 'MyModule::MyModule::MyClass' }, X::AdHoc,
    'the logical type name does not manufacture another package binding';

lives-ok { EVAL 'module Outer { class Inner {}; class Outer::Inner {} }' },
    'no false redeclaration';
throws-like { EVAL 'module Twice { class C {}; class C {} }' },
    X::Redeclaration, 'a real redeclaration is still rejected';
