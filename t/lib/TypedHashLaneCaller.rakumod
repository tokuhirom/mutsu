unit class TypedHashLaneCaller;
use TypedHashLaneBlock;

my %seen{Hash};

method go($k) { my %k = a => 1; %seen{$%k} = 1; put-any($k) }
