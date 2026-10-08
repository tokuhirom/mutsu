# Blob and Buf are shapes of the method table

`Blob`, `Buf` and the encoding buffers now dispatch through the one method table (ADR-11276 §8.3, §9.32). `elems`, `bytes`, `of`, `list`, `contents`, `reverse`, `Bool`, `gist`, `raku`, `Str`, the `read-*` accessors and `subbuf` are rows of both `Blob` and `Buf`, and the Buf mutators (`push`, `append`, `unshift`, `prepend`, `pop`, `shift`, `splice`, `reallocate`) are `Mut` rows of `Buf`, which is where Rakudo declares them. The cascades call the same functions for receivers without a shape.
