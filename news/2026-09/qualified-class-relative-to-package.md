# Qualified class declarations are relative to their package

`class MyModule::MyClass {}` inside `unit module MyModule` (or `module MyModule { }`) next to
`class MyClass {}` no longer reports a false `Redeclaration of symbol`. A qualified declaration
name inside a package is relative to that package, so the second class is
`MyModule::MyModule::MyClass`; only a `GLOBAL::` prefix is absolute. Bare lookup of
`MyModule::MyClass` from inside the package still resolves to the sibling class (tracked as a
follow-up).
