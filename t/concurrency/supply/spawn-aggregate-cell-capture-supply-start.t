use Test;

plan 5;

# ADR-12204 S2 step 2: `Supply.start`'s per-value call runs on a thread whose
# capture source is the callable, so the `@`/`%` lexicals it references are
# captured by their cells, with no lane entry (MUTSU_SPAWN_CAPTURE_CHECK oracle).

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

sub run-with(Str $level) {
    my %env = %*ENV;
    %env<MUTSU_SPAWN_CAPTURE_CHECK> = $level if $level.defined;
    my $p = run $*EXECUTABLE, '-e', $code, :out, :err, :env(%env);
    (out => $p.out.slurp(:close).trim, err => $p.err.slurp(:close).trim).Map
}

my $expect = '3';
my %off = run-with(Str);
is %off<out>, $expect, "writes reach the parent";

my %on = run-with("all");
is %on<out>, $expect, "oracle on: output unchanged";
like %on<err>, /'cell-captured @acc'/, "@acc is cell-captured";
like %on<err>, /'cell-captured %h'/, "%h is cell-captured";
unlike %on<err>, /'cell-captured-lane-write'/, "no lane write for a cell-captured name";
