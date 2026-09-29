use Test;

plan 4;

# A qualified class name declared inside a package is relative to that
# package: `class M::C` inside `module M` is `M::M::C`, so it does not
# redeclare the sibling `class C`.

module MyModule {
    class MyClass {}
    class MyModule::MyClass {}
}

is MyModule::MyClass.^name, 'MyModule::MyClass', 'plain class keeps its name';
is MyModule::MyModule::MyClass.^name, 'MyModule::MyModule::MyClass',
    'qualified declaration is relative to the enclosing package';

lives-ok { EVAL 'module Outer { class Inner {}; class Outer::Inner {} }' },
    'no false redeclaration';
throws-like { EVAL 'module Twice { class C {}; class C {} }' },
    X::Redeclaration, 'a real redeclaration is still rejected';
