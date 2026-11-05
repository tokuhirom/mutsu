use v6;
use lib 't/lib/ResSplit/lib';
use Test;

plan 3;

# From LLM::Prompts: `slurp(%?RESOURCES<prompts.json>)` -- the routine form
# reads the resource's file like the `.slurp` method form does.
use ResSplit;

my $r = strs();
my $expected = $r.slurp;
ok $expected.chars > 0, 'the method form reads the resource';
is slurp($r), $expected, 'the routine form reads the same content';
is slurp($r, :enc<utf8>), $expected, 'the routine form takes named arguments';
