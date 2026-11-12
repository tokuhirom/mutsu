use Test;

# A `%h := $hash` bind that reaches the variable by name (not through a slot
# of the binding frame) must bind the Hash itself, not a snapshot of it: a
# write through the source after the bind is seen through `%h`. EEL's fork
# restore (`%state := $_` in a RakuAST-built block) went through this path
# and its `:my` hash aliases drifted apart.

plan 3;

our %g;
sub bind-g($x) { %g := $x; $x<late> = 1; %g<late> }

my $c = {a => 1};
is bind-g($c), 1, 'a write through the source after the bind is seen through the bound name';

our %h;
sub bind-h($x) { %h := $x; %h<back> = 3 }
my $d = {b => 1};
bind-h($d);
is $d<back>, 3, 'a write through the bound name is seen through the source';

our %i;
sub bind-i($x) { %i := $x; $x<k> = 4; %i := $x; %i<k> }
is bind-i({}), 4, 'rebinding the same Hash keeps sharing it';
