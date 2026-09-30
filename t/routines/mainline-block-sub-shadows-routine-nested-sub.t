use Test;

# mutsu#10391: a mainline block's `my sub NAME` must not read the free
# variables of a same-named `my sub` declared inside a routine.

plan 3;

sub mk() {
    my $seq = 5;
    my sub reset() { $seq = 0 }
    my &c := { reset() };
    (&c, sub { $seq });
}
my ($c, $get) = mk();
$c();

{
    my $seq = 7;
    my sub reset() { $seq = 0 }
    reset();
    is $seq, 0, 'direct call in a block reaches the block\'s own $seq';
}
{
    my $seq = 8;
    my sub reset() { $seq = 0 }
    my &blk = { reset() };
    blk();
    is $seq, 0, 'call through a closure reaches the block\'s own $seq';
}
is $get(), 0, "the routine's own sub still writes its own \$seq";
