unit module UnitTypedLaneB;
my %seen{Hash};
my Str @log;
sub put-b($k) is export { %seen{$k} = 1; %seen.elems }
sub log-b($v) is export { @log.push($v); @log.elems }
