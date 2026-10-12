use Test;

# A Supply derived (`.map`) from a Supplier::Preserving while another thread
# emits: every value reaches the derived supply's first tap exactly once and in
# emission order. The derived supply's tap used to be registered before the
# source's backlog was carried over, so an emission racing that window was
# delivered twice (`a! a!`) or ahead of the backlog (`b! a!`) (#12831).

plan 1;

my %seen;
for ^1500 -> $i {
    my $supplier = Supplier::Preserving.new;
    my $p = start {
        my $c = $supplier.Supply.map(* ~ "!").Channel;
        ($c.receive, $c.receive)
    };
    my $spin = 0;
    $spin++ for ^($i % 150 * 20);
    $supplier.emit("a");
    $supplier.emit("b");
    %seen{(await $p).join(",")}++;
}
is-deeply %seen, %("a!,b!" => 1500),
    'emits racing the derived supply registration arrive once, in order';
