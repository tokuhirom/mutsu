# Reads `$*CU` from a routine of a module, as static's `fail-on-mismatch` does,
# and exports a type of its own (static's `CheckFailure`).
unit module CuReader;
class ReaderType is export(:types) { }
sub type-values($cu = $*CU) is export {
    my %seen;
    sub walk($n) {
        if $n ~~ RakuAST::Type::Simple {
            my $name = $n.name.canonicalize;
            %seen{$name} = (try $n.compile-time-value.^name) // "unresolved";
        }
        $n.visit-children(&walk);
    }
    walk($cu);
    %seen
}
