use Test;
plan 2;

my $s = Supply.new();
my $sum = 0;
react {
    whenever $s -> $x { $sum += $x; }
}
$s.emit(2);
$s.emit(3);

is $sum, 5, 'whenever reacts to emits';

my $s2 = Supply.new();
$s2.emit(1);
my $seen = 0;
react {
    whenever $s2 -> $x { $seen += $x; }
}

is $seen, 1, 'whenever sees existing values';
