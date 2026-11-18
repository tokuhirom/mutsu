use Test;

# A `.map` callback's `FIRST` runs once per closure clone, also when the
# block has ENTER/LEAVE/LAST phasers, which make mutsu run the body as a
# block scope (#18024: ParaSeq t/09 "FIRST phaser fired only once").

plan 6;

my @log;
my $b = { FIRST @log.push('F'); ENTER @log.push('E'); $_ };
is (^3).map($b).List, (0, 1, 2), 'values';
is @log.join, 'FEEE', 'FIRST once, before ENTER';

@log = ();
my $c = { FIRST @log.push('F'); LEAVE @log.push('L'); LAST @log.push('Z'); $_ * 2 };
is (^3).map($c).List, (0, 2, 4), 'values with LEAVE and LAST';
is @log.join, 'FLLLZ', 'FIRST once, LEAVE per call, LAST at the end';

my atomicint $first;
my atomicint $enter;
my $d = { FIRST $first⚛++; ENTER $enter⚛++; $_ * 2 };
my @r = (^100).map($d);
is $first, 1, 'FIRST counted once';
is $enter, 100, 'ENTER counted per call';
