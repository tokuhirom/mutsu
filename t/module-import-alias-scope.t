use v6;
use lib 't/lib';
use Test;

# A unit module's body currently executes in the caller's env. Its imports
# must remain visible to its own routines without leaving the imported aliases
# in the caller after the module load (tokuhirom/mutsu#7692).

plan 11;

my $issue7692-value = 'caller-value';
use Issue7692::Mid;

is i7692-mid(), '42/hi', 'a unit module routine sees its imported variable and class';
is i7692-nested(), '42', 'a nested sub inherits the unit module import scope';
is i7692-block(), '42/hi', 'a nested block inherits the unit module import scope';
is $issue7692-value, 'caller-value', 'a same-named caller variable is restored';
nok (try $issue7692-only).defined, 'an imported variable does not leak to the caller';
nok (try I7692Class.new.hi).defined, 'an imported class does not leak to the caller';
nok (try i7692-exported()).defined, 'a transitive imported sub does not leak to the caller';

is EVAL(q:to/CODE/), 'only-in-vars', 'a direct use still imports the variable';
    use Issue7692::Vars;
    ::('$issue7692-only')
CODE

is EVAL(q:to/CODE/), 'hi', 'a direct use still imports the class';
    use Issue7692::Vars;
    I7692Class.new.hi
CODE

is EVAL(q:to/CODE/), 'exported', 'a direct use still imports the sub';
    use Issue7692::Vars;
    i7692-exported()
CODE

is $issue7692-value, 42, 'a direct import installs the imported value in the caller';
