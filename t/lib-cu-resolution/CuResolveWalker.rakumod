# Walks `$*CU` from a routine of a module and reports how each type resolved.
unit module CuResolveWalker;
sub resolutions() is export {
    my %seen;
    sub w($n) {
        if $n ~~ RakuAST::Type::Simple {
            %seen{$n.name.canonicalize} = $n.is-resolved ?? $n.resolution.^name !! "unresolved";
        }
        $n.visit-children(&w) if $n.^can("visit-children");
    }
    w($*CU);
    %seen
}
