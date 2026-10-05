use v6;
use Test;
use lib 't/lib';
use RoutineLocalMultiLeak;

plan 1;

my $leaked = try EVAL 'routine-export-local-multi(1)';
nok $leaked.defined,
    'a multi declared by sub EXPORT does not leak into the importing compunit';
