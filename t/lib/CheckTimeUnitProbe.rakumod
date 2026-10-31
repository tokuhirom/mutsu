unit module CheckTimeUnitProbe;
use experimental :rakuast;

# Reads the unit being compiled (`$*CU`) from a module's own scope, as
# static::Analyzer does from its CHECK, and reports how a call resolves.
sub probe-sample(--> Int) is export { 1 }

sub probe-resolution(Str $name --> List) is export {
    my @found;
    my sub walk($node) {
        return unless $node ~~ RakuAST::Node;
        if $node ~~ RakuAST::Call::Name && $node.name.canonicalize eq $name {
            @found.push: ($node.is-resolved && $node.resolution.compile-time-value ~~ Routine).so;
        }
        $node.visit-children({ walk($_) });
    }
    walk($*CU);
    @found.List
}
