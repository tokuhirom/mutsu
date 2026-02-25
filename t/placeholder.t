use Test;
plan 11;

my $add = { $^a + $^b };
is $add(3, 4), 7, 'placeholder block with two args';

my @data = 3, 1, 2;
my @sorted = @data.sort({ $^a <=> $^b });
is @sorted.join(","), "1,2,3", 'sort with placeholder comparator';

my @nums = 1, 2, 3;
my @doubled = @nums.map({ $^x * 2 });
is @doubled.join(","), "2,4,6", 'map with placeholder var';

my @all = 1, 2, 3, 4;
my @evens = @all.grep({ $^n %% 2 });
is @evens.join(","), "2,4", 'grep with placeholder var';

my $sub = { $^b - $^a };
is $sub(10, 25), 15, 'placeholders sorted alphabetically';

sub rt68116 { 68116 }
is { &^x() }.( &rt68116 ), 68116, 'call-on invokes callable placeholder for sub';

proto mone(|) { * }
multi mone { 'one' }
is { &^x() }.( &mone ), 'one', 'call-on invokes callable placeholder for multi';

sub mixed-placeholder {
    [@_[0] // Nil, %_<y> // Nil, @^arr.elems, %^h<z>, &^cb()]
}
is-deeply mixed-placeholder([10, 20], { 40 }, {:z(30)}, 99, :x(2), :y(50)),
    [99, 50, 2, 30, 40],
    '@^/%^/&^ placeholders bind with types and @_/%_ capture leftovers';
dies-ok { mixed-placeholder(42, { 40 }, {:z(30)}, 99, :x(2), :y(50)) },
    '@^ placeholder rejects non-Positional values';
dies-ok { mixed-placeholder([10, 20], { 40 }, 42, 99, :x(2), :y(50)) },
    '%^ placeholder rejects non-Associative values';
dies-ok { mixed-placeholder([10, 20], 42, {:z(30)}, 99, :x(2), :y(50)) },
    '&^ placeholder rejects non-Callable values';
