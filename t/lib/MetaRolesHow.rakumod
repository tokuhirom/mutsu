# A custom HOW defined in a module that composes Rakudo's metaroles
# (UI::HTMLWindow's `class WindowHOW is Metamodel::ClassHOW does Metamodel::DefaultParent`).
unit module MetaRolesHow;

class WindowHOW is Metamodel::ClassHOW does Metamodel::DefaultParent is export { }
