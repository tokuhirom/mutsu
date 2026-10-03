use Test;

plan 4;

# `my %h .= &f` calls a routine as a method on the fresh variable, like the
# statement form `%h .= &f` (Pod::To::PDF's `my %opts .= &get-opts`).
sub get-opts(%h) { %h<a> = 1; %h }
my %opts .= &get-opts;
is-deeply %opts, %(a => 1), 'hash declaration';

sub double($x) { ($x // 21) * 2 }
my $n .= &double;
is $n, 42, 'scalar declaration, no arguments';

sub pair-up($x, $y) { "{$x.raku} $y" }
my $p .= &pair-up(3);
is $p, 'Any 3', 'with arguments';

my $s = 5;
$s .= &double;
is $s, 10, 'the statement form still works';
