unit class HashKeyLaneOwner;
my %seen{List} = ();
method put($k) { %seen{$k} = 1; %seen.elems }
