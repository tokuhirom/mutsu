use Test;

plan 2;

# Found via JSON::Pretty: its `sub EXPORT` maps `&to-json` to its own multi
# dispatcher, while its candidates call the `to-json` they imported from
# JSON::Fast. The one-slot `&name` env entry used to hand the module's own call
# the override, so `to-json` recursed until the stack ran out.
use lib 't/lib';
use ExportOverImported;

is shout(5), '<base(5)>', 'the importer sees the re-exported override';
is shout(Int), 'null', 'the override dispatches its other candidates';
