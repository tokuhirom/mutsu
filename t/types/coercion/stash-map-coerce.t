# A package stash is a Map of its symbols: `.Map` / `.Hash` coerce to them.
use Test;
use lib 't/lib';

plan 4;

use ExportStashBase;

my $m = ExportStashBase::EXPORT::ALL::.Map;
isa-ok $m, Map, '.Map of an export stash is a Map';
is $m.keys.sort, ('&check', '&run-named'), 'with the stash symbols as keys';
my $h = ExportStashBase::EXPORT::ALL::.Hash;
isa-ok $h, Hash, '.Hash of an export stash is a Hash';
is $h.keys.sort, ('&check', '&run-named'), 'with the same keys';
