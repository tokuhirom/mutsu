# Proc::Async output runs on the shared streaming decoder

Proc::Async stdout/stderr text is now decoded by the same streaming decoder
(`src/builtins/stream_decoder/`) that backs `Encoding::Decoder::Builtin` and the
`nqp::decoder*` ops, instead of a private UTF-8 splitter. Grapheme and CRLF
holdback across read boundaries come from the one state machine, and an
incomplete multibyte sequence at the end of the stream now quits the supply, as
Rakudo does. `Encoding::Registry.find(...).encoder` is now an
`Encoding::Encoder::Builtin` doing the `Encoding::Encoder` role (#11783).
IO::Handle and socket read paths are still on their own splitters.
