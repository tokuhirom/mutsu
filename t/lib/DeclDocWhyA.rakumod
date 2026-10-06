#| Adds one.
sub inc-a($x) is export { $x + 1 }
sub why-of-inc-a is export { &inc-a.WHY.Str }

#| A documented class.
class DeclDocWhyClass {
    #| A documented method.
    method m { 1 }
    #| First multi candidate.
    multi method mm(Int $x) { 1 }
    #| Second multi candidate.
    multi method mm(Str $x) { 2 }
}
sub why-of-class is export { DeclDocWhyClass.WHY.Str }
sub why-of-method is export { DeclDocWhyClass.^find_method('m').WHY.Str }

#| A documented role.
role DeclDocWhyRole { }

# Not documented, and named like a documented routine of the importer.
sub undocumented-in-module is export { 1 }
sub why-of-undocumented is export { &undocumented-in-module.WHY.defined }
