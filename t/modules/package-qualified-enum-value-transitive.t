use Test;

# An `our` enum declared in a package block installs its values in that
# package, so `PET::Msg::Ping` resolves in any file that has (transitively)
# loaded the module — not only in one that `use`s it directly. mutsu's
# compile-time module scan dropped the value as a lexical import of the
# intermediate module, and `when PET::Msg::Ping { ... }` then failed to parse
# ("needs parens to avoid gobbling block"). Reduced from Cro::WebSocket's
# Handler, which says `when Cro::WebSocket::Message::Ping`.

plan 3;

use lib 't/lib/PkgEnumTransitive';
use PET::Handler;

is PET::Handler.kind(PET::Msg.new(opcode => PET::Msg::Ping)), 'ping',
    'transitively loaded package-qualified enum value as a when matcher';
is PET::Handler.kind(PET::Msg.new(opcode => PET::Msg::Text)), 'other', 'other value';
is +PET::Msg::Ping, 9, 'the qualified value is usable in the importer too';
