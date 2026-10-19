unit module UseInBlockPackageVars;

our $n is export = 1;
our %h = a => 1;
our @l = 1;

our sub show { "$n {%h.sort.map(*.key).join(',')} {@l.join(',')} $UseInBlockPackageVars::n" }
