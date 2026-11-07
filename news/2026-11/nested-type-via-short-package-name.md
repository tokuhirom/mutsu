# A nested type is reachable through its package's short name

`use`ing a module whose `unit package M` declares `our class Packet::Sub { class Inner {} }`
lets the importer write `Packet::Sub`, and now `Packet::Sub::Inner` too: when a
qualified name does not resolve, its package resolves as a type under a longer
name and exactly one registered type is named `<outer>::Packet::Sub::Inner`, that
type is the answer, as in rakudo. Protocol::MQTT `t/04-client.rakutest` passes
13/13.
