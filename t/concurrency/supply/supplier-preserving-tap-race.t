use Test;

# A tap of a Supplier::Preserving supply while another thread emits: every
# value reaches the tap exactly once and in emission order. The tap used to be
# registered before the backlog was replayed, so an emission racing that window
# arrived twice (`a,a`), ahead of the backlog (`b,a`) or never (the receive
# below hung). The same holds through a live stage, which taps its preserving
# source per tap (ADR-18443).

plan 2;

for (False, True) -> $staged {
    my %seen;
    for ^1000 -> $i {
        my $supplier = Supplier::Preserving.new;
        my $p = start {
            my $c = Channel.new;
            my $supply = $supplier.Supply;
            $supply = $supply.map(* ~ "!") if $staged;
            $supply.tap(-> $v { $c.send($v) });
            ($c.receive, $c.receive)
        };
        my $spin = 0;
        $spin++ for ^($i % 150 * 20);
        $supplier.emit("a");
        $supplier.emit("b");
        await Promise.anyof($p, Promise.in(30));
        %seen{$p.status ~~ Kept ?? $p.result.join(",") !! 'lost'}++;
        last unless $p.status ~~ Kept;
    }
    my $want = $staged ?? "a!,b!" !! "a,b";
    is-deeply %seen, %($want => 1000),
        "a {$staged ?? 'staged ' !! ''}tap racing emits gets each value once, in order";
}
