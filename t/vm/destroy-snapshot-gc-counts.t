use v6;
use Test;

# Pin for #11334 (fixed by #12319): a garbage cycle of instances whose class
# has a DESTROY, each holding a still-live Pair inside a Capture attribute.
# When the cycle collector reclaimed such an instance, the DESTROY snapshot
# kept the Capture box (and the Pair handles inside it) alive although trial
# deletion had already deducted those handles, so dropping the snapshot later
# drove the live Pair's GC strong count below zero: "Gc::drop strong-count
# underflow" (a debug assertion; seen on Cro::HTTP's pool threads in
# http-session-inmemory.rakutest). The child runs with a collect armed every
# 64 candidates so the reclaim path is exercised on every run.

plan 3;

my $code = q:to/END/;
    class Node {
        has $.c;
        has $.other is rw;
        method DESTROY { }
    }
    my @keep;
    for ^1500 {
        my $pair = (key => "v$_");
        @keep.push: $pair;
        my $a = Node.new(c => \($pair, :k($pair)));
        my $b = Node.new(c => \($pair));
        $a.other = $b;
        $b.other = $a;
        my @junk = (1..5).map({ $_ => $pair });
    }
    for ^100 { my @x = (^50).map({ $_ => $_ }) }
    my $n = 0;
    $n += .value.chars for @keep;
    @keep = ();
    for ^100 { my @x = (^50).map({ $_ => $_ }) }
    say "chars $n";
    END

my %env = %*ENV;
%env<MUTSU_GC> = 'on';
%env<MUTSU_GC_EVERY_CANDIDATE> = '64';
my $proc = run $*EXECUTABLE, '-e', $code, :out, :err, :%env;
my $out = $proc.out.slurp(:close);
my $err = $proc.err.slurp(:close);

is $out.trim, 'chars 6390', 'the shared Pairs stay intact across DESTROY reclaims';
is $proc.exitcode, 0, 'the child exits cleanly';
nok $err ~~ /'strong-count underflow'/, 'no GC strong-count underflow';
