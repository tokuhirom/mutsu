# A module file's exports are found under a differently named package block

`Cro/WebSocket/Message/Opcode.rakumod` declares `package Cro::WebSocket::Message { enum Opcode is export
(...) }`: a package named differently from the file. rakudo imports whatever the file exports, wherever
it was declared; mutsu looked its enum values up only under the requested module name (or its `unit`
package), so `Binary` and `Text` stayed `Any` in the importer and `Cro::WebSocket::Message.new` died
with "expected Opcode but got Any". A file with no `unit` declaration and one top-level package block
under another name now records that package, and the export lookup consults it. Cro::WebSocket's
`websocket-handler`, `-message-parser` and `-message-serializer` suites went from dying to matching raku.
