use ImportedEnumOpcode;
class ImportedEnumSerializer::Binary {
    method serialize($message) { $message.opcode = Binary; 1 }
}
