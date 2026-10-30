use Test;

# Cro::WebSocket::Message::Opcode's file declares `package
# Cro::WebSocket::Message { enum Opcode is export (...) }`, a package named
# differently from the file. rakudo imports what the file exports whatever
# package block holds it; mutsu looked only under the requested module name,
# so `Binary`/`Text` stayed undeclared (Any) in the importer.

plan 5;

use lib 't/lib';
use ExportsFromOtherPackage;

is Binary.Int, 2, 'an exported enum value of a differently named package block is imported';
is Text.Int, 1, 'another one';
is Close.raku, 'Opcode::Close', 'its raku form';
is shout('hi'), 'HI!', 'an exported sub of that package is imported too';

class Msg { has Opcode $.op is rw }
is Msg.new(op => Binary).op, Binary, 'the enum type is usable as an attribute type';
