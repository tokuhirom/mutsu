use Test;

# `.VAR.name` of an element container (#19080): an element that does not
# exist yet is named after its subscript, and an element of an array a `$`
# variable holds lives in an anonymous container called `element`.

plan 11;

my @b;
is @b[5].VAR.name, '@b[5]', 'missing array element: @b[5]';
my @c = 1;
is @c[0].VAR.name, '@c', 'existing array element: @c';
is @c[3].VAR.name, '@c[3]', 'past the end: @c[3]';
my $i = 2;
is @b[$i].VAR.name, '@b[2]', 'variable index';

my %h;
is %h<a>.VAR.name, q[%h{'a'}], 'missing hash element';
is %h{1}.VAR.name, q[%h{'1'}], 'missing hash element, Int key';
my %g = a => 1;
is %g<a>.VAR.name, '%g', 'existing hash element: %g';

my $x = [1, 2];
is $x[0].VAR.name, 'element', 'element of an itemized array';
is $x[7].VAR.name, 'element[7]', 'missing element of an itemized array';
is @b[1][2].VAR.name, 'element[2]', 'chained subscript of a missing element';
is [1, 2][5].VAR.name, 'element[5]', 'missing element of an array literal';
