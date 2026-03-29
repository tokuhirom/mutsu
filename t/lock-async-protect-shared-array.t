use Test;

plan 1;

my $n           = 100;
my $n-threads   = 4;
my $target-lock = Lock::Async.new;
my @target;

await start {
    for ^$n -> $i {
        $target-lock.protect: {
            push @target, $i;
        }
    }
} xx $n-threads;

is [+](@target), $n-threads * [+](^$n),
    'Lock::Async.protect preserves shared array pushes across threads';
