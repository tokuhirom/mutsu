use Test;

plan 5;

my @grouped;
for ('xy' ~~ /(.)(.)/) { @grouped.push($_.Str) }
is-deeply @grouped, ['x', 'y'], 'a grouped smartmatch iterates positional captures';

my @pointy;
for 'xy' ~~ /(.)(.)/ -> $capture { @pointy.push($capture.Str) }
is-deeply @pointy, ['x', 'y'], 'a direct smartmatch binds each positional capture';

my @method;
for 'xy'.match(/(.)(.)/) { @method.push($_.Str) }
is-deeply @method, ['x', 'y'], 'a Match returned by a method iterates its captures';

my @empty;
for ('xy' ~~ /xy/) { @empty.push($_) }
is @empty.elems, 0, 'a Match without positional captures iterates zero times';

my $itemized = 'xy' ~~ /(.)(.)/;
my @scalar;
for $itemized { @scalar.push($_.Str) }
is-deeply @scalar, ['xy'], 'a scalar container keeps its Match as one item';
