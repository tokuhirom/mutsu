use Test;

plan 8;

# ADR-12204 S2: an `@`/`%` lexical that a `start` block references and that
# lives in a shared cell is captured by that cell and gets no name-keyed lane
# entry. Pins mutsu's guarantee (no lane write for a cell-captured name), via
# the MUTSU_SPAWN_CAPTURE_CHECK oracle, and the Raku-visible results.

my $code = q:to/RAKU/;
my @cellarr;
my %cellhash;
await start { @cellarr.push(1); @cellarr.push(2); %cellhash<k> = 3 };
await start { @cellarr[0] = 9; %cellhash<j> = 4 };
my @whole = 1, 2, 3;
await start { @whole = (7, 8) };
say @cellarr.raku, ' ', %cellhash.sort.map(*.raku).join(','), ' ', @whole.raku;
RAKU

sub run-with(Str $level) {
    my %env = %*ENV;
    %env<MUTSU_SPAWN_CAPTURE_CHECK> = $level if $level.defined;
    my $p = run $*EXECUTABLE, '-e', $code, :out, :err, :env(%env);
    (out => $p.out.slurp(:close).trim, err => $p.err.slurp(:close).trim).Map
}

my $expect = '[9, 2] :j(4),:k(3) [7, 8]';
my %off = run-with(Str);
is %off<out>, $expect, "child writes reach the parent through the cell";

my %on = run-with("all");
is %on<out>, $expect, "oracle on: output unchanged";
like %on<err>, /'cell-captured @cellarr'/, "@cellarr is cell-captured";
like %on<err>, /'cell-captured %cellhash'/, "%cellhash is cell-captured";
like %on<err>, /'cell-captured @whole'/, "@whole is cell-captured";
unlike %on<err>, /'cell-captured-lane-write'/, "no lane write for a cell-captured name";

# Two bindings of one name stay separate cells.
sub f(Int $n) {
    my @acc;
    await start { @acc.push($n) };
    @acc
}
is-deeply (f(1), f(2)), ([1], [2]), "a re-declared aggregate is a different cell per call";

my @outer = 1;
sub g { my @outer = 5; await start { @outer.push(6) }; @outer }
is-deeply (g(), @outer), ([5, 6], [1]), "a callee's aggregate does not clobber the caller's";
