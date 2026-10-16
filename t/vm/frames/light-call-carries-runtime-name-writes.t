# A write whose target name exists only at run time
# (`CALLER::CALLER::.BIND-KEY`, `$::($n) = v`), made two light calls below the
# frame that owns the variable, reaches that frame: the light call paths carry
# it across each return explicitly, as the named path does, instead of relying
# on their plain-lexical return merge (ADR-12529 §3, slice 36). The fixture runs
# with TRIR off so the routines in between take the light path. Checked
# against rakudo.
use Test;

plan 2;

my $fixture = $?FILE.IO.parent(3).add('fixtures/light-call-runtime-name-writes.raku').Str;
my %env = %*ENV;
%env<MUTSU_TRIR> = 'off';
my $proc = run($*EXECUTABLE, $fixture, :out, :err, :%env);
my $out = $proc.out.slurp(:close);
my $err = $proc.err.slurp(:close);

is $proc.exitcode, 0, 'the fixture runs clean' or diag "stderr was:\n$err";
is $out, q:to/END/, 'each runtime-named write reaches the owning frame';
    bound => 41
    stored => 9
    named => 5
    END
