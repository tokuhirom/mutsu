use Test;

plan 6;

class MQ { has $.n }

# `temp $x = Other.new` rebinds `$x`; restoring it must not rewrite the object
# bound inside the scope, which anything stored there still refers to.
my $b = MQ.new(:n<outer>);
my $kept;
{ temp $b = MQ.new(:n<inner>); $kept = $b; }
is $kept.n, 'inner', 'a bare block keeps what was copied out of the scope';
is $b.n, 'outer', 'the variable is restored';

my $c = MQ.new(:n<outer>);
my %seen;
if True { temp $c = MQ.new(:n<inner>); %seen<k> = $c; }
is %seen<k>.n, 'inner', 'a hash element stored inside the scope keeps its value';
is $c.n, 'outer', 'restored after an if block';

# Mutating the same object is still undone: the restore writes its attributes back.
class Counter { has $.n is rw }
my $counter = Counter.new(:n(1));
{ temp $counter.n = 5; is $counter.n, 5, 'a temp attribute assignment is visible inside'; }
is $counter.n, 1, 'and undone afterwards';
