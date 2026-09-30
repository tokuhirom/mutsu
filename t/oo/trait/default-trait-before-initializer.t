use Test;

plan 22;

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

# `state` and `our` declarations take the same default-first route (#10256).
state @s is default(7) = Nil, Any;
is @s[0], 7, 'state: a Nil list item uses the declared default';
ok @s[1] === Any, 'state: an explicit Any list item stays Any';

our @p is default(7) = Nil, Any;
is @p[0], 7, 'our: a Nil list item uses the declared default';
ok @p[1] === Any, 'our: an explicit Any list item stays Any';
ok @GLOBAL::p[1] === Any, 'our: the package variable holds the same container';
is @GLOBAL::p[5], 7, 'our: the package variable keeps the default';

package P { our @x is default(3) = Nil, Any; }
ok @P::x[1] === Any, 'our in a package: explicit Any stays Any';
is @P::x[0], 3, 'our in a package: Nil uses the default';

my $inits = 0;
sub reenter {
    state @r is default(7) = do { ++$inits; Nil }, Any;
    @r.push(@r.elems);
    @r.raku ~ ' ' ~ @r[10]
}
reenter();
my $second = reenter();
is $inits, 1, 'state: the initializer runs only on the first entry';
is $second, '[7, Any, 2, 3] 7', 'state: the defaulted container persists across entries';

state Int @typed is default(5) = Nil, 2;
is @typed.raku, 'Array[Int].new(5, 2)', 'state: a typed defaulted array keeps its element type';

my @seen;
for ^2 {
    state @loop is default(3) = Nil, Any;
    @seen.push(@loop[1].raku);
    @loop[1] = Nil;
}
is @seen.join(','), 'Any,3', 'state in a loop: initialized once, then the default applies';
{
    state @after is default(4) = Nil, Any;
    ok @after[0] == 4 && @after[1] === Any, 'state in a bare block initializes like my';
}
