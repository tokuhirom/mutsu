use Test;

# #8798: an exported wrapper loses to a same-named imported proto after its
# first call. `ExportedWrapperSameNameProto`'s exported `wrapped-thing`
# internally `use`s `ExportedWrapperSameNameProtoInner`, whose own exported
# `wrapped-thing` is a `proto`/`multi` family with the SAME name — the shape
# the real `User::grent`/`P5getgrnam` ecosystem distributions hit
# (`getgrgid`/`getgrnam`/`getgrent`). The first call correctly shadows the
# imported family inside the wrapper body and restores the wrapper's own
# identity when it returns; a regression here left the imported family's
# registry keys leaked past the call, so a SECOND external call resolved the
# dependency's raw routine instead of the wrapper.
plan 3;

use lib 't/lib';
use ExportedWrapperSameNameProto;

is wrapped-thing(42), 'wrapped:raw:42', 'first call runs the wrapper';
is wrapped-thing(42), 'wrapped:raw:42', 'second call still runs the wrapper';
is wrapped-thing(42), 'wrapped:raw:42', 'third call still runs the wrapper';
