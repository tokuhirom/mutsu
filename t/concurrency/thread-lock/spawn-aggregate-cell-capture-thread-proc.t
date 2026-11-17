use Test;

# The capture oracle these tests once consulted is gone (ADR-16595, #17837):
# no user lexical can reach the lineage store, so only the results remain.

plan 1;

# ADR-12204 S2 step 2: `Thread.start`'s body and a `Proc::Async` reader tap
# callback are the capture sources of the threads that run them, so an `@`/`%`
# lexical they reference is captured by its cell, with no lane entry.

my $code = q:to/RAKU/;
my @ran;
my %got;
my $t = Thread.start({ @ran.push(1); %got<t> = 1 });
$t.finish;
my @lines;
my $proc = Proc::Async.new($*EXECUTABLE, '-e', 'say "a"; say "b"');
$proc.stdout.tap({ @lines.push($_.chomp) });
await $proc.start;
say @ran.raku, ' ', %got.raku, ' ', @lines.join("\n").lines.sort.join(",");
RAKU

sub run-with() {
    my %env = %*ENV;
    my $p = run $*EXECUTABLE, '-e', $code, :out, :err, :env(%env);
    (out => $p.out.slurp(:close).trim, err => $p.err.slurp(:close).trim).Map
}

my $expect = '[1] {:t(1)} a,b';
my %off = run-with();
is %off<out>, $expect, "writes reach the parent";

