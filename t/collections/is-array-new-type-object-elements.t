use v6.d;
use Test;

plan 6;

# An `is Array` subclass takes type objects as elements, like Array.new does.
class MyA is Array { }

my $m = MyA.new(Any, 'b');
is $m.elems, 2, 'a leading type object is an element';
ok $m[0] === Any, '... and stays the type object';
is $m[1], 'b', '... followed by the second element';
is MyA.new(Int).elems, 1, 'a lone type object is one element';
ok MyA.new(1, Any)[1] === Any, 'a trailing type object keeps working';

class MyI is Int { }
throws-like { MyI.new(Int) }, Exception, 'a numeric subclass still refuses a type object';
