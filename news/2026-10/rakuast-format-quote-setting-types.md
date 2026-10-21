# Format quotes retain their processors across RakuAST

`q:o` and `qq:format` convert to `RakuAST::QuotedString` with the measured
`format` processor and ordinary literal or interpolation segments. Lowering
uses the same format-construction expression as the parser. Constructed nodes
therefore produce the same callable Format as source quotes.

Processor ordering retains Rakudo's distinction between `q:w:o` and `q:o:w`.
Format installation replaces preceding word processors, while a later word
processor passes a List to the strict format parameter and raises its ordinary
binding exception. Negating an already installed format processor retains it.
Plain `qw`, `qww` and interpolating word quotes keep a List even for one word;
angle words and `:val` retain their scalar/allomorph behavior.

The setting catalog gains generated 6.e additions measured against the same
Rakudo's 6.d setting. `Formatter::Syntax` remains a type operand rather than
turning into an argument-less routine call. A keyword boundary also prevents
the `format` adverb from being mistaken for a `for` loop label.

The Formatter grammar captures the `s`, `c`, `o`, `O` and `u` directive symbols
and anchors its TOP, matching the measured core grammar. The old assertion
that a string directive has no symbol is corrected without removing coverage.
Quoted-string lowering, parser word-result construction and runtime quote
processing are split into focused modules.
