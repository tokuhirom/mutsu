use Test;

# A sigilless pointy-block parameter takes the same tail as every other
# parameter shape: `is` traits and a default. Proxee writes its STORE block as
# `-> $, \v is raw { ... }`; the unconsumed `is raw` used to fail the block at
# its opening brace, and with it the whole `Proxy.new` call.

plan 7;

my $raw = -> \v is raw { v };
is $raw(5), 5, 'a sigilless pointy parameter may carry is raw';

my $pair = -> $x, \v is raw { v + $x };
is $pair(1, 2), 3, 'and it composes with an earlier sigilled parameter';

my $defaulted = -> \v = 7 { v };
is $defaulted(), 7, 'a sigilless pointy parameter may carry a default';
is $defaulted(9), 9, 'and the default gives way to an argument';

my $plain = -> \v { v };
is $plain(3), 3, 'a bare sigilless pointy parameter still works';

# `is raw` on a sigilless parameter really does alias, so writing through it
# reaches the caller's container.
my $n = 1;
my $bump = -> \v is raw { v = v + 1 };
$bump($n);
is $n, 2, 'is raw on a sigilless pointy parameter aliases the argument';

# The shape Proxee actually uses.
my $store-target;
my $p := Proxy.new(
    FETCH => -> $ { $store-target },
    STORE => -> $, \v is raw { $store-target = v },
);
$p = 42;
is $store-target, 42, 'Proxy.new with a sigilless raw STORE parameter works';
