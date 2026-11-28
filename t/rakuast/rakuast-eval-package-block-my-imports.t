use Test;
use experimental :rakuast;
use MONKEY-SEE-NO-EVAL;

plan 1;

my $source = q[use Test;
module Provider { sub helper is export { 1 } }
my @seen;
module Consumer { import Provider; @seen = MY::.keys.grep(*.starts-with('&')).sort }
my @block;
{ import Provider; @block = MY::.keys.grep(*.starts-with('&')).sort }
@seen.join(',') ~ '|' ~ @block.join(',')];

is EVAL($source.AST), '&helper|&helper',
    'package and nested block MY:: each list their own imports inside AST EVAL';
