use Test;

# The capture oracle these tests once consulted is gone (ADR-16595, #17837):
# no user lexical can reach the lineage store, so only the results remain.

plan 1;

# ADR-12204 S2 step 2: the closure handed to scheduler cue (and a threaded
# `tap` / `whenever`) is the capture source of the thread that runs it, so an `@`/`%` lexical
# it references is captured by its cell (no name-keyed lane entry), like a
# `start` block's. Pins mutsu's guarantee.

my $code = q:to/RAKU/;
my @tapped;
my %seen;
my $s = Supplier.new;
$s.Supply.tap({ @tapped.push($_); %seen{$_} = 1 });
$s.emit($_) for 1..3;
$s.done;
my @cued;
my $p = Promise.new;
$*SCHEDULER.cue({ @cued.push(42); $p.keep(1) });
await $p;
say @tapped.elems, ' ', %seen.elems, ' ', @cued.raku;
RAKU

sub run-with() {
    my %env = %*ENV;
    my $p = run $*EXECUTABLE, '-e', $code, :out, :err, :env(%env);
    (out => $p.out.slurp(:close).trim, err => $p.err.slurp(:close).trim).Map
}

my %off = run-with();
is %off<out>, '3 3 [42]', "callbacks' writes reach the parent";

