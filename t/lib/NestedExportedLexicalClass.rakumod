unit module NestedExportedLexicalClass;

# A `my class ... is export` declared inside a block of the unit, not at its
# top level. rakudo exports it just the same.
if True {
    my class NestedExported is export { method hi { "hi from nested" } }
}
