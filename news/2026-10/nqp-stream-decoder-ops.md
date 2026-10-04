# One streaming decoder behind the `nqp::decoder*` ops and `Encoding::Decoder::Builtin`

The ten stream-decoding `nqp::` ops of #11503 (part of the `nqp::` coverage
campaign #11488) are implemented, plus `decodertakecharseof`:
`decoderconfigure`, `decodersetlineseps`, `decoderaddbytes`,
`decodertakechars`, `decodertakecharseof`, `decodertakeavailablechars`,
`decodertakeallchars`, `decodertakeline`, `decoderbytesavailable`,
`decoderempty` and `decodertakebytes`.

They run over a new state machine, `builtins::stream_decoder`, which models
MoarVM's decoder as three queues — undecoded bytes, pending text and final
chars — instead of the old `Encoding::Decoder`, which kept one byte array and
re-decoded it on every call. That old shape could not answer the ops
faithfully, and it gave wrong answers of its own:

- `consume-available-chars` now holds back the last grapheme, as MoarVM's
  NFG normalizer does, so a combining mark arriving in the next chunk joins
  its base character instead of being handed out alone (`"a"` + `"\x[301]b"`
  gives `"á"`, `"b"`, not `"a"`, `"\x[301]b"`).
- Decoding is lazy and stops at the line separator or the requested char
  count, so the bytes behind a header line stay raw for
  `consume-exactly-bytes` — the shape Cro's HTTP parsers rely on.
- Malformed input is an error (`Malformed UTF-8 near byte ff`), and bytes
  that do not form a whole character at the end of the stream are reported
  as MoarVM reports them instead of becoming U+FFFD.
- The undecoded bytes live in a `Buf` whose storage drops consumed bytes in
  O(1) amortized, and the text queues are moved rather than copied between
  calls, so taking lines one at a time is linear.

The decoder class is now `Encoding::Decoder::Builtin` (doing the
`Encoding::Decoder` role), as in Rakudo, with `.new($encoding,
:translate-nl)` and `consume-exactly-chars`. Its methods run the same
routines over the same object state as the ops, so a program can mix the
two, and a decoder held in an attribute (`$!decoder.add-bytes(...)`) now
keeps its state — the old native methods only updated a decoder stored in a
variable.
