use Test;

plan 5;

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

sub run-with(Str $level) {
    my %env = %*ENV;
    %env<MUTSU_SPAWN_CAPTURE_CHECK> = $level if $level.defined;
    my $p = run $*EXECUTABLE, '-e', $code, :out, :err, :env(%env);
    (out => $p.out.slurp(:close).trim, err => $p.err.slurp(:close).trim).Map
}

my $expect = '[1] {:t(1)} a,b';
my %off = run-with(Str);
is %off<out>, $expect, "writes reach the parent";

my %on = run-with("all");
is %on<out>, $expect, "oracle on: output unchanged";
like %on<err>, /'cell-captured @ran'/, "Thread.start body cell-captures @ran";
like %on<err>, /'cell-captured @lines'/, "the Proc::Async tap callback cell-captures @lines";
unlike %on<err>, /'cell-captured-lane-write'/, "no lane write for a cell-captured name";
