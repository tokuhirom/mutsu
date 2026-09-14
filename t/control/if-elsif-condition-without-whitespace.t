use Test;

plan 2;

# Red 0.2.5 uses `elsif($attr.rw)` in MetamodelX::Red::Relationship.
sub choose(Int:D $value --> Str:D) {
    if $value == 0 {
        'if'
    } elsif($value == 1) {
        'elsif'
    } else {
        'else'
    }
}

is choose(1), 'elsif', 'elsif accepts a condition without intervening whitespace';
is choose(2), 'else', 'the following else branch remains reachable';
