use Test;
use experimental :rakuast;
use MONKEY-SEE-NO-EVAL;

plan 2;

is EVAL(q[my @seen; for 1..3 { last if $_ == 2; @seen.push($_) }; @seen.join(',')].AST),
    '1', 'a core last remains loop control after AST EVAL';

my $source = q[use lib 't/lib'; use LastWrapperSub;
my @seen = (1..5).map({ last if $_ == 3; $_ });
my @more = (1..5).map({ last() if $_ == 3; $_ });
for 1..3 { next if $_ == 2 }
@seen.join(',') ~ '|' ~ @more.join(',') ~ '|' ~ $LastWrapperSub::counter];
is EVAL($source.AST), '1,2|1,2|12',
    'imported last and next stay routine calls after AST EVAL';
