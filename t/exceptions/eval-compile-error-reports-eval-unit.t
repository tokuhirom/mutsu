use Test;

# #18870: an uncaught compile error raised inside `EVAL` is reported against
# the EVAL's own unit -- its `EVAL_N` name, the line within the EVAL text and
# that text's excerpt -- not against the calling file's line 1.

plan 10;

my $code = q:to/END/;
say 1;
EVAL "say 2;\nmy \$y = 3 4";
END

my $p = run $*EXECUTABLE, '-e', $code, :err, :out;
my $err = $p.err.slurp(:close);
is $p.out.slurp(:close), "1\n", 'code before the EVAL ran; the EVAL text did not';
ok $p.exitcode != 0, 'the uncaught compile error fails the program';
like $err, /'Error while compiling EVAL_' \d+/, 'the report names the EVAL unit';
like $err, /'at EVAL_' \d+ ':2'/, 'the line is the one within the EVAL text';
like $err, /'my $y = 3' \s* '4'/, 'the excerpt is the EVAL text';
unlike $err, /'say 1;'/, 'the calling file is not quoted';

# `Str.AST` parses its string as a unit of its own, the same way.
my $q = run $*EXECUTABLE, '-e', "say 1;\nQ[my \$x = 1 2].AST;", :err, :out;
my $qerr = $q.err.slurp(:close);
$q.out.slurp(:close);
like $qerr, /'at EVAL_' \d+ ':1'/, "a .AST parse failure names the string's unit";
like $qerr, /'my $x = 1' \s* '2'/, "a .AST parse failure quotes the string's text";

# Leading blank lines of the EVAL text count toward the line (they used to be
# trimmed off, shifting `.line` and the report).
my $line = (try { EVAL "\n\nmy \$x = 1 2"; Nil }) // $!.line;
is $line, 3, '.line counts the EVAL text\'s leading blank lines';
my $b = run $*EXECUTABLE, '-e', "EVAL q\{\n\nmy \$x = 1 2\};", :err, :out;
$b.out.slurp(:close);
like $b.err.slurp(:close), /'at EVAL_' \d+ ':3'/, 'so does the report';
