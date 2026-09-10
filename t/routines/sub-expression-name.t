use v6;
use Test;

plan 8;

# PLAN §8.13: a named sub in expression position keeps its name and — like any
# sub declaration in Raku — installs itself lexically.

my $s = sub foo { 1 };
is $s.name, 'foo', 'expression-form named sub keeps its .name';
is $s(), 1, 'the stored value is callable';
is &foo.name, 'foo', 'the name is installed lexically too';
is foo(), 1, 'the installed sub is callable by name';

is (sub baz { 2 }).name, 'baz', 'parenthesized expression form keeps the name';
my &g = sub bar { 3 };
is &g.name, 'bar', 'bound to a &-var the name survives';
is g(), 3, 'and it is callable through the binding';

# The statement form still works as before.
sub plain { 4 }
is &plain.name, 'plain', 'statement-form name is unchanged';

done-testing;
