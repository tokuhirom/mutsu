# Fixture for t/modules/package-qualified-enum-value-transitive.t (shape of
# Cro::WebSocket::Message::Opcode): an exported enum declared inside a
# package block.
package PET::Msg {
    enum Opcode is export (:Text(1), :Ping(9));
}
