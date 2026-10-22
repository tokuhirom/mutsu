unit class TypedHashLaneUnit;

my %seen{Hash};

method put($key) { %seen{$key} = 1; %seen.elems }
