use lib 't/lib';
use Test;
use ExportedProtoDispatchee;

plan 3;

# A module's `my proto` family extended by `BEGIN &proto.add_dispatchee(...)`
# keeps its own `my multi` when the proto is exported (#14500).
is &tj-out.candidates.map(*.signature.raku).sort.join(' | '),
    ':($x, :$pretty = Bool::True, *%o) | :(@_, *%_)',
    'the exported dispatcher holds the multi and the dispatchee';
is tj-out(<a b c>.Array), '[<a>,<b>,<c>]', 'the multi wins for an Array';
is tj2(5), '<5>', 'the dispatchee is also a plain exported sub';
