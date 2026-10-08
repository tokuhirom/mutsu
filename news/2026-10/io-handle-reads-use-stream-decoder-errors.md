# IO::Handle text reads decode through the shared streaming decoder

`decode_utf8_handle_text` (behind `.get`, `.lines`, `.slurp`, `.readchars`)
now decodes through `builtins::stream_decoder::decode_stream`, the same state
machine as `Encoding::Decoder::Builtin`, `nqp::decoder*` and Proc::Async. A
malformed or truncated file now fails with Rakudo's decoder messages
(`Malformed UTF-8 near byte ff`, `Incomplete character near bytes e3 81 at the
end of a stream`) instead of the one-shot `Blob.decode` wording (#11783).
Socket read paths and the handle line splitter are still separate.
