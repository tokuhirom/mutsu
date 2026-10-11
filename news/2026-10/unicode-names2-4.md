# unicode_names2 4.0 and multi-word name aliases

`unicode_names2` is bumped to 4.0. The crate now matches multi-word Unicode
name aliases loosely (`byteordermark`), which Rakudo does not: it accepts
`BYTE ORDER MARK` case-insensitively but rejects `byte-order-mark`. mutsu keeps
the Rakudo behaviour, so the crate-parity unit test no longer compares those
aliases, and `t/types/string/unicode-name-alias-spelling.t` pins the Rakudo
spelling rules instead.
