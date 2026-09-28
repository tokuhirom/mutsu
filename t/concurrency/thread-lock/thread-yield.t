use Test;

plan 3;

is Thread.yield, Nil, 'Thread.yield returns Nil';

my $count = 0;
for ^10 {
    Thread.yield;
    $count++;
}
is $count, 10, 'execution continues after yielding';

my $thread = Thread.new(code => { 1 });
dies-ok { $thread.yield }, 'yield requires the Thread type object';
