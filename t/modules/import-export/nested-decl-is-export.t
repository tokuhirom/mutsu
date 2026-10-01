use Test;

# `is export` exports a declaration from any depth of a package's lexical
# region -- a routine body, a branch, a bare block, a nested package or class
# -- at compile time, whether or not the enclosing code ever runs (#10543).

plan 14;

use lib 't/lib';

# Constants and enums in an inline module.
module M8 { sub f { constant k8 is export = 8; enum E8 is export <e8a e8b> } }
{
    import M8;
    is k8, 8, 'constant nested in a routine body';
    is e8b.value, 1, 'enum nested in a routine body';
}

# Routines in an inline module.
{
    module M1 { sub f { sub g1 is export { 'g1' } } }
    import M1;
    is g1(), 'g1', 'sub nested in a routine body';
}
{
    module M2 { if True { sub g2 is export { 'g2' } } }
    import M2;
    is g2(), 'g2', 'sub nested in a branch';
}
{
    module M4 { module N { sub g4 is export { 'g4' } } }
    import M4;
    is g4(), 'g4', 'sub of a nested module is exported from the outer one';
}
{
    module M5 { module N { sub g5 is export { 'g5' } } }
    import M5::N;
    is g5(), 'g5', '... and from the nested module itself';
}
{
    module M6 { class K { sub g6 is export { 'g6' } } }
    import M6;
    is g6(), 'g6', 'sub of a class inside a module is exported from the module';
}
{
    module M7 {
        module N {
            multi sub g7(Int) is export { 'int' }
            multi sub g7(Str) is export { 'str' }
        }
    }
    import M7;
    is g7(1) ~ g7('a'), 'intstr', 'every multi candidate reaches the outer module';
}

# Declarations nested in routine bodies of a module file.
{
    use NestedDeclExports;
    # Bound first: the importer's parse does not know the name is a term.
    my $k = nested-k;
    is $k + 1, 6, 'constant from a routine body of a module file';
    is nex2.value, 1, 'enum from a routine body of a module file';
    is NestedEx.^name, 'NestedEx', 'the enum type itself';
    is NestedCC.m, 42, 'lexical class from a routine body of a module file';
    ok with-class() === NestedCC, "the exported class is the routine's own type object";
}
{
    use NestedDeclExports :extra;
    my $tagged = nested-tagged;
    is $tagged, 'tagged', 'a nested declaration keeps its export tag';
}
