use Test;

plan 9;

my @nested is default(42) = [Nil];
ok @nested[0] === Any, 'an inner array decays Nil to its own Any';
is @nested[3], 42, 'the outer default still applies to missing indices';

my @flat is default(7) = Nil, Any;
is @flat[0], 7, 'a Nil list item uses the declared default';
ok @flat[1] === Any, 'an explicit Any list item stays Any';

my @events;
sub record($event) { @events.push($event); @events.elems }
my @ordered is default(record('default')) = [record('initializer')];
is @events.join(','), 'default,initializer', 'the trait argument runs before the initializer';
is @ordered[0], 2, 'the initializer observes the trait argument side effect';
is @ordered[3], 1, 'the declared default is evaluated once';

my @outer is default(9) = 1;
{
    my @outer is default(4) = [Any];
    ok @outer[0] === Any, 'a shadowing declaration keeps its explicit Any';
}
is @outer[3], 9, 'the outer container retains its own default';
