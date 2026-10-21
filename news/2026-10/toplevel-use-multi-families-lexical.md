# A top-level `use`'s multi families bind through the unit's lexical set

The `LexicalImportCapture::top_level` carve-out is gone: the multi families a
unit-level `use` imports are recorded in the unit's lexical import set like a
block's, instead of staying registry aliases. `rw` and raw parameters still see
the caller's container (`t/routines/dispatch/toplevel-use-multi-raw-rw-arg.t`).
Part of #14140.
