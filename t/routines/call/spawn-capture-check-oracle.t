use Test;

plan 5;

# MUTSU_SPAWN_CAPTURE_CHECK (ADR-12204 S1): an env-gated oracle that names the
# aggregate lanes a spawned block's capture analysis does not cover, and leaves
# behaviour alone.

my $code = q:to/RAKU/;
my @s; my %r;
class W { method go { @s.push(2) } }
my $pr = start { 1 };
await start { W.new.go };
await start { %r<a> = 1 };
say @s.elems, %r.elems;
RAKU

sub run-with(Str $level) {
    my %env = %*ENV;
    %env<MUTSU_SPAWN_CAPTURE_CHECK> = $level if $level.defined;
    my $p = run $*EXECUTABLE, '-e', $code, :out, :err, :env(%env);
    (out => $p.out.slurp(:close).trim, err => $p.err.slurp(:close).trim).Map
}

my %off = run-with(Str);
is %off<out>, "11", "unset: the program runs";
is %off<err>, "", "unset: nothing is reported";

my %on = run-with("1");
is %on<out>, "11", "set: the program output is unchanged";
like %on<err>, /'uncaptured-lane-write name=@s op=set'/,
    "a lane write by a registered method's body (invisible to the capture analysis) is reported";
unlike %on<err>, /'name=%r'/, "a name the spawned block itself captures is not reported";
