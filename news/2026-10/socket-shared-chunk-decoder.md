# `IO::Socket::Async` decodes on the shared streaming decoder

A socket's text `Supply` had its own UTF-8 splitter next to the streaming
decoder that `Proc::Async` already used (#11783). Both now run on one
`ChunkDecoder` (`stream_decoder::chunks`), which holds back a split sequence
or grapheme until the next read. A malformed byte quits the `Supply` with
rakudo's `Malformed UTF-8 near byte ff`, and an incomplete sequence at the end
of the stream quits with `Incomplete character near bytes d0 at the end of a
stream`. An `:enc<ascii>` socket rejects a high byte. `IO::CatHandle` already
read through the `IO::Handle` decoder and matches rakudo.
