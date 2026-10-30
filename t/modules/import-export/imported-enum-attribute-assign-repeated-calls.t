use lib $*PROGRAM.parent(3).add('lib');
use Test;
use ImportedEnumOpcode;
use ImportedEnumSerializer;

# Cro::WebSocket: a module method doing `$message.opcode = Binary` (an enum
# value imported into the module) ran every body-blob call. The assigned
# variable's own cell was stored into the attribute, so the second store
# wrote the cell into itself and the third read of `Binary` never returned.

plan 6;

class Msg { has Opcode $.opcode is rw; }
my $m = Msg.new;
my $s = ImportedEnumSerializer::Binary.new;

for 1..5 -> $i {
    $s.serialize($m);
}
is $m.opcode, Binary, 'the attribute holds the enum value after five calls';
is Binary.raku, 'Opcode::Binary', 'the imported enum value itself is intact';

$m.opcode = Text;
is Binary.raku, 'Opcode::Binary', 'assigning the attribute from here does not change the enum value';
$s.serialize($m);
is $m.opcode, Binary, 'serializing again sets it back';
$m.opcode = Ping;
is Binary.raku, 'Opcode::Binary', 'and the enum value stays intact';
$s.serialize($m);
is $m.opcode.raku, 'Opcode::Binary', 'a call after another assignment still works';
