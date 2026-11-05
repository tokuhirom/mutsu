# X::Seq::Consumed message has rakudo's line breaks

The message is three lines in rakudo (`...another Seq (you`, `might solve...`,
`the Seq into an array)`); mutsu printed it on one line. Found by the raku-doc
differential sweep (`Type/Seq.rakudoc`).
