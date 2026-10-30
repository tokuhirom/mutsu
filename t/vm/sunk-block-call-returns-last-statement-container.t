use Test;

plan 7;

# A Block hands back the container its last statement evaluated to, so a sunk
# call does not sink the value inside it; a Routine decontainerizes its
# implicit result, and a fresh Failure is sunk. (Statements at the top level:
# a `try`/`lives-ok` block makes Failures fatal, which is another matter.)
sub fl { Failure.new("boom") }
my $z;

my &c = { $z = fl() };
c();
pass 'a block ending in a scalar assignment sinks nothing';
my &d = { $z };
d();
pass 'a block ending in a scalar variable sinks nothing';
my &e = { my $y = fl() };
e();
pass 'a block ending in a scalar declaration sinks nothing';
my &p = { $z = run($*EXECUTABLE, '-e', 'exit 3') };
p();
pass 'an unsuccessful Proc stored by the block is not thrown';

my &f = { fl() };
dies-ok { f(); 1 }, 'a fresh Failure from the last call is sunk';
sub s1 { $z = fl() }
dies-ok { s1(); 1 }, 'a routine decontainerizes its result and sinks it';
my &w = sub { $z = fl() };
dies-ok { w(); 1 }, 'an anonymous sub is a routine too';
