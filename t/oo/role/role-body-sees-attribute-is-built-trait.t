use v6;
use Test;

# Found via SBOM::CycloneDX: a role body runs while the class is composed, and
# its `$?CLASS.^attributes.grep(*.is_built)` must already honour `is built(False)`.

plan 2;

role R {
    my @attributes is List = $?CLASS.^attributes.grep(*.is_built);
    method names { @attributes.map(*.name).List }
}
class A does R {
    has $.a;
    has @.b is built(False);
    has $.c is built(False) = 5;
    has $!d is built;
}
is A.names, ('$!a', '$!d'), 'role body filters on is built(False)';
is A.^attributes.grep(*.is_built).map(*.name).List, ('$!a', '$!d'), 'same after composition';
