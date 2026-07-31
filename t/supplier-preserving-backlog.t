use v6;
use Test;

plan 4;

# Supplier::Preserving buffers values emitted while no tap listens and
# replays them to the next tap (plain Supplier drops them).

# 1) Direct tap after emit.
{
    my $p = Supplier::Preserving.new;
    $p.emit("a");
    my @got;
    $p.Supply.tap: { @got.push($_) };
    is @got.join(","), "a", 'backlog replays to a late direct tap';
    $p.emit("b");
    is @got.join(","), "a,b", 'live emits still flow after the replay';
}

# 2) whenever inside a supply block subscribing after the emit.
{
    my $p = Supplier::Preserving.new;
    $p.emit("x");
    my $s = supply { whenever $p.Supply { emit $_ x 2 } }
    my @got;
    $s.tap: { @got.push($_) };
    is @got.join(","), "xx", 'backlog replays through a supply-block whenever';
}

# 3) The backlog is delivered once, not to every tap.
{
    my $p = Supplier::Preserving.new;
    $p.emit("once");
    my @first;
    my @second;
    $p.Supply.tap: { @first.push($_) };
    $p.Supply.tap: { @second.push($_) };
    is @first.join(",") ~ "|" ~ @second.join(","), "once|",
        'only the first tap consumes the preserved backlog';
}
