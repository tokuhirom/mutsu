use Test;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;

plan 3;

my $source = q:to/END/;
use Test;
$=finish.trim
=finish
the finish section
END

is EVAL($source.AST), 'the finish section', 'AST EVAL retains the finish section';
is EVAL($source), 'the finish section', 'string EVAL exposes its own finish section';
ok !$=finish.defined, 'the EVAL finish section does not leak into the caller';
