use Test;

plan 4;

# `.lines` on an on-demand supply (a `supply { }` block) splits the emitted
# chunks into lines when it is tapped, and keeps the newlines under :!chomp.
my $in = supply { emit "1..2\nok 1\n"; emit "not ok 2\n" };
is-deeply $in.lines.list, ("1..2", "ok 1", "not ok 2"), 'on-demand .lines splits chunks';
is-deeply $in.lines(:!chomp).list, ("1..2\n", "ok 1\n", "not ok 2\n"), ':!chomp keeps the newlines';

# A sub declared in a supply block emits into that block's supply, also when
# it is called from a `whenever` over a chained on-demand supply.
my $c = supply {
    sub e($x) { emit "C:$x" }
    whenever $in.lines -> $line { e($line) }
}
is-deeply $c.list, ("C:1..2", "C:ok 1", "C:not ok 2"), 'emit from a nested sub inside whenever';

my $d = supply {
    my @buffer;
    sub emit-reset($line) { emit "E:" ~ $line.chomp; @buffer = () }
    whenever $in.lines(:!chomp) -> $line { emit-reset $line }
    LEAVE { emit-reset @buffer.join('') if @buffer }
}
is-deeply $d.list, ("E:1..2", "E:ok 1", "E:not ok 2"), 'TAP parse-stream shape';
