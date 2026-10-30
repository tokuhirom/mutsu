package Exports::Other::Name {
    enum Opcode is export (:Text(1), :Binary(2), :Close(8));
    sub shout($s) is export { $s.uc ~ '!' }
}
