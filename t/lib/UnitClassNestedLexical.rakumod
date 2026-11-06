unit class UnitClassNestedLexical;
my $n = 0;
my @log;
my %seen;
my %by-name;

class Inner {
    method name { 'inner' }
    method count { ++$n }
    method log(Str $s) { @log.push($s); @log.elems }
    method seen(Str $k) { ++%seen{$k} }
    method set-seen(Str $k) { %seen{$k} = 10; %seen{$k} }
    method !by { ++%by-name{ self.name } }
    method by { self!by }
}

sub make is export { Inner.new }
sub state-report is export { "$n @log.elems() %seen.elems()" }
