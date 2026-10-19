use Test;

class NamedData {
    has %.named;
}

my $direct = NamedData.new(:named(Map.new((level => 'info'))));
is $direct.named.^name, 'Hash', 'a Map passed by name initializes a plain Hash attribute';
is $direct.named<level>, 'info', 'the Hash attribute receives the Map entries';

sub make-data(|c) { NamedData.new(:named(c.hash)) }
my $captured = make-data(:a(1));
is $captured.named.^name, 'Hash', 'a Map from Capture.hash initializes a plain Hash attribute';
is $captured.named<a>, 1, 'the Hash attribute receives the captured named arguments';

done-testing;
