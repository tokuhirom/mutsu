unit module NestedExportDecls;

# `is export` exports from any depth: rakudo lands this operator in the
# module's export list although it is declared inside a routine body.
sub setup {
    sub infix:<nested-cat>($a, $b) is export { "$a:$b" }
}

# A lexical constant and a non-exported enum inside a routine body are private
# to it; an importer's own routines of the same names must stay callable.
sub private-decls {
    my constant nested-private-const = 9;
    enum NestedPrivateEnum <nested-private-value>;
}
