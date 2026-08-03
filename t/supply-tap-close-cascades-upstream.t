use v6;
use Test;

# Closing a tap closes the supply block that produced it, which closes the
# `whenever` subscriptions inside it, which closes *their* sources — all the way
# down to the original Supplier. mutsu closed only the block the tap was taken
# on: the upstream `whenever` stayed subscribed, so its CLOSE phaser never fired
# and values kept reaching the (closed) tap callback.
#
# `Cro::Service.stop` is exactly this shape (`$!service-tap.close` on a pipeline
# whose bottom is a TCP listener), so a stopped server kept serving and a second
# server on the same port never saw a request.

plan 5;

my $src = Supplier.new;
my @closed;
my @got;

my $upstream = supply {
    whenever $src.Supply -> $v { emit $v * 10 }
    CLOSE { @closed.push('upstream') }
};
my $mid = supply {
    whenever $upstream -> $v { emit $v + 1 }
    CLOSE { @closed.push('mid') }
};

my $tap = $mid.tap(-> $v { @got.push($v) });
$src.emit(1);
is @got.join(","), "11", "a value flows through both blocks";

$tap.close;
is @closed.join(","), "upstream,mid",
    "closing the tap closes the whole chain, source-first";

$src.emit(2);
is @got.join(","), "11", "no value reaches the closed tap afterwards";

# A single-level supply block still closes exactly once.
my $src2 = Supplier.new;
my @closed2;
my $one = supply {
    whenever $src2.Supply -> $v { emit $v }
    CLOSE { @closed2.push('one') }
};
my @got2;
my $tap2 = $one.tap(-> $v { @got2.push($v) });
$src2.emit('a');
$tap2.close;
$src2.emit('b');
is @closed2.join(","), "one", "a one-level block's CLOSE fires exactly once";
is @got2.join(","), "a", "and it stops receiving";

done-testing;
