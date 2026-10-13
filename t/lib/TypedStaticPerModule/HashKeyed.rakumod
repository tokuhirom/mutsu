# The same static names as ListKeyed, with other types (#12903).
unit class TypedStaticPerModule::HashKeyed;
my %seen{Hash};
my Str %counts;
method put($k) { %seen{$k} = 1; %seen.elems }
method count($k) { %counts<n> = $k; %counts<n> }
