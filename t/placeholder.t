use Test;
plan 5;

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
