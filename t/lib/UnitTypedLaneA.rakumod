unit module UnitTypedLaneA;
my %seen{List};
my Int @log;
sub put-a($k) is export { %seen{$k} = 1; %seen.elems }
sub log-a($v) is export { @log.push($v); @log.elems }
sub local-a($k) is export { my %seen{Str}; %seen{$k} = 1; %seen.keys.map(*.^name).join }
