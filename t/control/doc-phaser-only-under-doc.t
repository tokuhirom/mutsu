use Test;

# `DOC BEGIN`/`DOC CHECK`/`DOC INIT` is a statement prefix: the phaser runs only
# under `--doc` and is a no-op in an ordinary run. mutsu used to parse `DOC` as a
# separate bareword statement, so the phaser ran in every run and
# `DOC INIT { ... }` was two statements with no separator between them.

plan 4;

my $out = '';
my $ran = False;
EVAL 'DOC INIT { $ran = True }; $out ~= "main"';
nok $ran, 'DOC INIT does not run without --doc';
is $out, 'main', 'the statement after DOC INIT still runs';

EVAL 'DOC BEGIN { $ran = True }';
nok $ran, 'DOC BEGIN does not run without --doc';

my $code = 'DOC INIT { say "alive"; exit }; say "main"';
my $proc = run $*EXECUTABLE, '--doc', '-e', $code, :out;
is $proc.out.slurp(:close).trim, 'alive', 'DOC INIT runs under --doc';
