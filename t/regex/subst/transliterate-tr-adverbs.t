use Test;

{
    temp $_ = 'meows';
    is TR:d/eox/E/, 'mEws', 'TR:d/// deletes extra source chars';
    is $_, 'meows', 'standalone TR does not mutate $_';
}

{
    temp $_ = 'meeeeooowwweeees';
    is TR:d:c:s/ewZ//, 'eeeewwweeee', 'TR:d:c:s/// supports stacked adverbs';
    is $_, 'meeeeooowwweeees', 'stacked standalone TR keeps original $_';
}

my $s1 = 'hello';
$s1 ~~ TR/e/a/;
is $s1, 'hello', 'smartmatch TR leaves LHS unchanged';

my $s2 = 'meows';
$s2 ~~ TR:d/eox/E/;
is $s2, 'meows', 'smartmatch TR with :d leaves LHS unchanged';

done-testing;
