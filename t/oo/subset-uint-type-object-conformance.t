use v6;
use Test;

# Found via SBOM::CycloneDX: `UInt ~~ Cool` was False, so a generic
# "is it a Cool type?" test treated a UInt attribute as an object to .Hash.

plan 8;

ok UInt ~~ Cool, 'UInt does Cool';
ok UInt ~~ Int, 'UInt is an Int';
ok UInt ~~ Numeric, 'UInt does Numeric';
nok UInt ~~ Str, 'UInt is not a Str';

subset P of UInt where * > 0;
ok P ~~ Cool, 'subset of UInt does Cool';
ok P ~~ UInt, 'subset of UInt is a UInt';
ok P ~~ Int, 'subset of UInt is an Int';
my $t := UInt;
ok $t ~~ Cool, 'through a variable';
