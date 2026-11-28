use v6;
use Test;

plan 9;

$_ = 5;
my $read = { $_ };
$_ = 6;
is $read(), 6, 'a block reads a later assignment to its creator topic';

my $write = { $_ = 7 };
$write();
is $_, 7, 'a block assignment reaches its creator topic';

$_ = 5;
({ $_ = 7 })();
is $_, 7, 'an immediately called block also writes its creator topic';

sub outer-topic {
    $_ = 5;
    my $block = { $_ = 8 };
    $block();
    $_
}
is outer-topic(), 8, 'a routine sees a nested block topic assignment';

$_ = 1;
my $from-loop = { $_ = 9 };
for ^1 { $from-loop() }
is $_, 9, 'a block called from a loop writes its creation topic';

$_ = 10;
my $first = { $_ };
for 20 { is $_, 20, 'the loop binds its own topic' }
is $first(), 10, 'a later topicalizer leaves the captured binding intact';

my @loop-blocks;
for 1, 2 { @loop-blocks.push({ $_ }) }
is-deeply [@loop-blocks[0](), @loop-blocks[1]()], [1, 2],
    'closures from separate loop iterations retain separate topic bindings';

$_ = 11;
my $from-given = { $_ = 12 };
given 20 { $from-given() }
is $_, 12, 'a block called from given writes its creation topic';
