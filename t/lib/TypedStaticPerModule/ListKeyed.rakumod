# A class-body object hash keyed by List (#12903).
unit class TypedStaticPerModule::ListKeyed;
my %seen{List};
my Int %counts;
method put($k) { %seen{$k} = 1; %seen.elems }
method count($k) { %counts<n> = $k; %counts<n> }
