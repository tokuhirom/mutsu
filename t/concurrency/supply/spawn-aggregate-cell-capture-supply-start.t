use Test;

# The capture oracle these tests once consulted is gone (ADR-16595, #17837):
# no user lexical can reach the lineage store, so only the results remain.

plan 1;

# ADR-12204 S2 step 2: `Supply.start`'s per-value call runs on a thread whose
# capture source is the callable, so the `@`/`%` lexicals it references are
# captured by their cells, with no lane entry.

my $code = q:to/RAKU/;
my @acc; my %h;
my $sup = Supplier.new;
my $r = $sup.Supply.start({ @acc.push($_); %h{$_} = 1; $_ });
my @got;
$r.tap({ @got.push($_) });
$sup.emit($_) for 1..3;
my $deadline = now + 10;
sleep 0.01 until @got.elems == 3 || now > $deadline;
say @got.elems;  # not @acc/%h: Supply.start emits the inner Supplies before the bodies finish
RAKU

sub run-with() {
    my %env = %*ENV;
    my $p = run $*EXECUTABLE, '-e', $code, :out, :err, :env(%env);
    (out => $p.out.slurp(:close).trim, err => $p.err.slurp(:close).trim).Map
}

my $expect = '3';
my %off = run-with();
is %off<out>, $expect, "writes reach the parent";

