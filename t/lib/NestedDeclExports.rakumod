unit module NestedDeclExports;

# Each `is export` declaration below sits in a routine body that never runs:
# Rakudo exports it while compiling the declaration.
sub with-constant { constant nested-k is export = 5; }
sub with-enum { enum NestedEx is export <nex1 nex2> }
sub with-class is export { my class NestedCC is export { method m { 42 } }; NestedCC }
sub with-tagged { constant nested-tagged is export(:extra) = 'tagged' }
