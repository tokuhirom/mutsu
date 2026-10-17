# The built-in encodings' methods are method rows

`Encoding::Builtin`, its encoder and its streaming decoder answered their methods through by-name
`match`es behind a by-name gate. They are now method-table rows; the decoder's rows edit its state
in its own cell, as the `nqp::decoder*` ops do. `Encoding::Builtin.raku` renders its name, and a
UTF-8 encoder's `encode-chars` answers a `utf8` buffer, both as in Rakudo (ADR-11276 §9.156).
