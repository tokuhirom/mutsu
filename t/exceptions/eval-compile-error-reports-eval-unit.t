use Test;

plan 7;

# An uncaught compile error raised inside `EVAL` is reported against the EVAL's
# own unit (`EVAL_N`), its line within the EVAL text and that text's excerpt,
# not the calling file's line 1 (#18870).

my $script = q:to/CODE/;
say 1;
EVAL q{

my $x = 1 2};
CODE

my $proc = run $*EXECUTABLE, '-e', $script, :out, :err;
my $out = $proc.out.slurp(:close);
my $err = $proc.err.slurp(:close);
is $out, "1\n", 'the code before the EVAL ran';
like $err, /'Error while compiling ' \S* 'EVAL_'/, 'the error names the EVAL unit';
like $err, /'Two terms in a row'/, 'the message';
like $err, /'EVAL_' \d+ ':3'/, 'the line within the EVAL text, leading blank lines counted';
like $err, /'------>' \s* 'my $x = 1' .* '2'/, 'the excerpt is the EVAL text';
unlike $err, /'say 1;'/, 'not the calling file';

my $line = (try { EVAL "\n\nmy \$x = 1 2"; Nil }) // $!.line;
is $line, 3, '.line counts the leading blank lines';
