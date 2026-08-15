//! Generated data table for [`super::native_method_row`] -- see that module's
//! doc comment for what this is and how it was produced. Kept in its own file
//! so the ~340 literal rows do not push the logic/tests in
//! `native_method_row.rs` past the repo's 500-line-per-file guideline.

/// `(owner, name, arity_bits, flag_bits)`.
#[rustfmt::skip]
pub(super) const RAW_ROWS: &[(&str, &str, u8, u8)] = &[
    ("Str", "chars", 1, 9),
    ("Str", "codes", 1, 9),
    ("Str", "comb", 7, 9),
    ("Str", "chomp", 1, 9),
    ("Str", "chop", 3, 9),
    ("Str", "contains", 2, 9),
    ("Str", "ends-with", 2, 8),
    ("Str", "fc", 1, 9),
    ("Str", "flip", 1, 9),
    ("Str", "index", 2, 9),
    ("Str", "indices", 8, 12),
    ("Str", "lc", 1, 9),
    ("Str", "lines", 3, 9),
    ("Str", "match", 8, 12),
    ("Str", "ords", 1, 9),
    ("Str", "pred", 1, 9),
    ("Str", "rindex", 2, 9),
    ("Str", "samecase", 2, 9),
    ("Str", "samemark", 2, 9),
    ("Str", "split", 6, 9),
    ("Str", "starts-with", 2, 8),
    ("Str", "substr", 6, 9),
    ("Str", "succ", 1, 9),
    ("Str", "tc", 1, 9),
    ("Str", "trim", 1, 9),
    ("Str", "trim-leading", 1, 9),
    ("Str", "trim-trailing", 1, 9),
    ("Str", "uc", 1, 9),
    ("Str", "words", 3, 9),
    ("Str", "wordcase", 1, 9),
    ("Str", "NFC", 1, 9),
    ("Str", "NFD", 1, 9),
    ("Str", "NFKC", 1, 9),
    ("Str", "NFKD", 1, 9),
    ("Str", "encode", 1, 9),
    ("Str", "uniparse", 1, 9),
    ("Str", "unimatch", 6, 9),
    ("Str", "uniprops", 3, 9),
    ("Str", "parse-names", 1, 9),
    ("Str", "parse-base", 2, 9),
    ("Str", "subst", 8, 12),
    ("Str", "subst-mutate", 8, 10),
    ("Str", "substr-rw", 8, 12),
    ("Str", "substr-eq", 4, 8),
    ("Str", "trans", 8, 12),
    ("Str", "IO", 8, 12),
    ("Str", "Numeric", 1, 8),
    ("Str", "Int", 1, 9),
    ("Str", "Num", 1, 8),
    ("Str", "Rat", 3, 9),
    ("Str", "Bool", 1, 9),
    ("Str", "Str", 3, 9),
    ("Str", "gist", 1, 9),
    ("Str", "raku", 1, 9),
    ("Str", "elems", 1, 9),
    ("Str", "fmt", 7, 9),
    ("Int", "abs", 1, 9),
    ("Int", "ceiling", 1, 9),
    ("Int", "floor", 1, 9),
    ("Int", "round", 3, 9),
    ("Int", "sign", 1, 9),
    ("Int", "sqrt", 1, 9),
    ("Int", "log", 3, 9),
    ("Int", "log10", 1, 9),
    ("Int", "exp", 3, 9),
    ("Int", "roots", 2, 9),
    ("Int", "is-prime", 1, 9),
    ("Int", "chr", 1, 9),
    ("Int", "base", 6, 9),
    ("Int", "polymod", 8, 13),
    ("Int", "expmod", 4, 9),
    ("Int", "pred", 1, 9),
    ("Int", "succ", 1, 9),
    ("Int", "Numeric", 1, 9),
    ("Int", "Int", 1, 9),
    ("Int", "Num", 1, 8),
    ("Int", "Rat", 3, 9),
    ("Int", "Bool", 1, 9),
    ("Int", "Str", 3, 9),
    ("Int", "gist", 1, 9),
    ("Int", "raku", 1, 9),
    ("Num", "abs", 1, 9),
    ("Num", "ceiling", 1, 9),
    ("Num", "floor", 1, 9),
    ("Num", "round", 3, 9),
    ("Num", "sign", 1, 9),
    ("Num", "sqrt", 1, 9),
    ("Num", "log", 3, 9),
    ("Num", "log10", 1, 9),
    ("Num", "exp", 3, 9),
    ("Num", "roots", 2, 9),
    ("Num", "is-prime", 1, 9),
    ("Num", "chr", 1, 9),
    ("Num", "base", 6, 9),
    ("Num", "polymod", 8, 13),
    ("Num", "expmod", 4, 9),
    ("Num", "pred", 1, 9),
    ("Num", "succ", 1, 9),
    ("Num", "Numeric", 1, 9),
    ("Num", "Int", 1, 9),
    ("Num", "Num", 1, 9),
    ("Num", "Rat", 3, 9),
    ("Num", "Bool", 1, 9),
    ("Num", "Str", 3, 9),
    ("Num", "gist", 1, 9),
    ("Num", "raku", 1, 9),
    ("Rat", "abs", 1, 9),
    ("Rat", "ceiling", 1, 9),
    ("Rat", "floor", 1, 9),
    ("Rat", "round", 3, 9),
    ("Rat", "sign", 1, 9),
    ("Rat", "sqrt", 1, 9),
    ("Rat", "log", 3, 9),
    ("Rat", "log10", 1, 9),
    ("Rat", "exp", 3, 9),
    ("Rat", "roots", 2, 9),
    ("Rat", "is-prime", 1, 9),
    ("Rat", "chr", 1, 9),
    ("Rat", "base", 6, 9),
    ("Rat", "polymod", 8, 13),
    ("Rat", "expmod", 4, 9),
    ("Rat", "pred", 1, 9),
    ("Rat", "succ", 1, 9),
    ("Rat", "Numeric", 1, 9),
    ("Rat", "Int", 1, 9),
    ("Rat", "Num", 1, 8),
    ("Rat", "Rat", 3, 9),
    ("Rat", "Bool", 1, 9),
    ("Rat", "Str", 3, 9),
    ("Rat", "gist", 1, 9),
    ("Rat", "raku", 1, 9),
    ("Complex", "abs", 1, 9),
    ("Complex", "ceiling", 1, 9),
    ("Complex", "floor", 1, 9),
    ("Complex", "round", 3, 9),
    ("Complex", "sign", 1, 9),
    ("Complex", "sqrt", 1, 9),
    ("Complex", "log", 3, 9),
    ("Complex", "log10", 1, 9),
    ("Complex", "exp", 3, 9),
    ("Complex", "roots", 2, 9),
    ("Complex", "is-prime", 1, 9),
    ("Complex", "chr", 1, 9),
    ("Complex", "base", 4, 9),
    ("Complex", "polymod", 8, 13),
    ("Complex", "expmod", 4, 9),
    ("Complex", "pred", 1, 9),
    ("Complex", "succ", 1, 9),
    ("Complex", "Numeric", 1, 9),
    ("Complex", "Int", 1, 9),
    ("Complex", "Num", 8, 12),
    ("Complex", "Rat", 3, 9),
    ("Complex", "Bool", 1, 9),
    ("Complex", "Str", 3, 9),
    ("Complex", "gist", 1, 9),
    ("Complex", "raku", 1, 9),
    ("List", "elems", 1, 9),
    ("List", "end", 1, 9),
    ("List", "keys", 1, 8),
    ("List", "values", 1, 8),
    ("List", "kv", 1, 8),
    ("List", "pairs", 1, 8),
    ("List", "antipairs", 1, 8),
    ("List", "join", 3, 9),
    ("List", "map", 8, 10),
    ("List", "grep", 8, 10),
    ("List", "first", 1, 8),
    ("List", "sort", 1, 8),
    ("List", "reverse", 1, 8),
    ("List", "rotate", 8, 10),
    ("List", "unique", 1, 9),
    ("List", "repeated", 1, 9),
    ("List", "squish", 1, 8),
    ("List", "flat", 7, 9),
    ("List", "eager", 8, 12),
    ("List", "lazy", 1, 9),
    ("List", "head", 3, 9),
    ("List", "tail", 3, 9),
    ("List", "skip", 8, 12),
    ("List", "push", 8, 10),
    ("List", "pop", 8, 10),
    ("List", "shift", 8, 10),
    ("List", "unshift", 8, 10),
    ("List", "splice", 8, 10),
    ("List", "append", 8, 10),
    ("List", "prepend", 8, 10),
    ("List", "classify", 8, 10),
    ("List", "categorize", 8, 10),
    ("List", "min", 1, 8),
    ("List", "max", 1, 8),
    ("List", "minmax", 1, 8),
    ("List", "minpairs", 8, 12),
    ("List", "maxpairs", 8, 12),
    ("List", "sum", 1, 8),
    ("List", "pick", 3, 9),
    ("List", "roll", 3, 9),
    ("List", "permutations", 1, 9),
    ("List", "combinations", 3, 9),
    ("List", "rotor", 8, 10),
    ("List", "batch", 2, 9),
    ("List", "produce", 8, 10),
    ("List", "reduce", 8, 10),
    ("List", "Bool", 1, 9),
    ("List", "Str", 3, 9),
    ("List", "gist", 1, 9),
    ("List", "raku", 1, 9),
    ("List", "Numeric", 1, 8),
    ("List", "Int", 1, 8),
    ("List", "Array", 1, 9),
    ("List", "List", 1, 9),
    // ADR-0019 E2b: `List`/`Array` rows for names absent from `LIST_METHODS`
    // (`builtin_type_method_names`'s candidate source for these two owners),
    // hand-probed against a real `Value::array` sample (2026-08-10) the same
    // way the `Match` rows above were -- `list`/`item`/`Slip`/`WHICH`/
    // `AT-POS`/etc. are real recognized names the original 14-slice
    // candidate list never included, not a probing gap. `dynamic` is
    // deliberately `Array`-only (not `List`): the cascade's own guard
    // (`methods_0arg/mod.rs`) restricts `.dynamic` to non-`List`-kind Array
    // values. Verified by `array_list_extra_rows_are_backed_by_the_cascade`
    // in `native_method_row.rs`.
    ("List", "list", 1, 8),
    ("List", "item", 1, 8),
    ("List", "Slip", 1, 8),
    ("List", "cache", 1, 0),
    ("List", "sink", 1, 8),
    ("List", "invert", 1, 8),
    ("List", "WHICH", 1, 0),
    ("List", "AT-POS", 2, 8),
    ("List", "EXISTS-POS", 2, 8),
    ("List", "is-lazy", 1, 8),
    ("List", "Capture", 1, 8),
    ("List", "hyper", 1, 8),
    ("List", "race", 1, 8),
    ("List", "Supply", 1, 8),
    ("Array", "elems", 1, 9),
    ("Array", "end", 1, 9),
    ("Array", "keys", 1, 8),
    ("Array", "values", 1, 8),
    ("Array", "kv", 1, 8),
    ("Array", "pairs", 1, 8),
    ("Array", "antipairs", 1, 8),
    ("Array", "join", 3, 9),
    ("Array", "map", 8, 10),
    ("Array", "grep", 8, 10),
    ("Array", "first", 1, 8),
    ("Array", "sort", 1, 8),
    ("Array", "reverse", 1, 8),
    ("Array", "rotate", 8, 10),
    ("Array", "unique", 1, 9),
    ("Array", "repeated", 1, 9),
    ("Array", "squish", 1, 8),
    ("Array", "flat", 7, 9),
    ("Array", "eager", 8, 12),
    ("Array", "lazy", 1, 9),
    ("Array", "head", 3, 9),
    ("Array", "tail", 3, 9),
    ("Array", "skip", 8, 12),
    ("Array", "push", 8, 10),
    ("Array", "pop", 8, 10),
    ("Array", "shift", 8, 10),
    ("Array", "unshift", 8, 10),
    ("Array", "splice", 8, 10),
    ("Array", "append", 8, 10),
    ("Array", "prepend", 8, 10),
    ("Array", "classify", 8, 10),
    ("Array", "categorize", 8, 10),
    ("Array", "min", 1, 8),
    ("Array", "max", 1, 8),
    ("Array", "minmax", 1, 8),
    ("Array", "minpairs", 8, 12),
    ("Array", "maxpairs", 8, 12),
    ("Array", "sum", 1, 8),
    ("Array", "pick", 3, 9),
    ("Array", "roll", 3, 9),
    ("Array", "permutations", 1, 9),
    ("Array", "combinations", 3, 9),
    ("Array", "rotor", 8, 10),
    ("Array", "batch", 2, 9),
    ("Array", "produce", 8, 10),
    ("Array", "reduce", 8, 10),
    ("Array", "Bool", 1, 9),
    ("Array", "Str", 3, 9),
    ("Array", "gist", 1, 9),
    ("Array", "raku", 1, 9),
    ("Array", "Numeric", 1, 8),
    ("Array", "Int", 1, 8),
    ("Array", "Array", 1, 9),
    ("Array", "List", 1, 9),
    ("Array", "list", 1, 8),
    ("Array", "item", 1, 8),
    ("Array", "Slip", 1, 8),
    ("Array", "cache", 1, 0),
    ("Array", "sink", 1, 8),
    ("Array", "invert", 1, 8),
    ("Array", "WHICH", 1, 8),
    ("Array", "AT-POS", 2, 8),
    ("Array", "EXISTS-POS", 2, 8),
    ("Array", "is-lazy", 1, 8),
    ("Array", "Capture", 1, 8),
    ("Array", "dynamic", 1, 8),
    ("Array", "hyper", 1, 8),
    ("Array", "race", 1, 8),
    ("Array", "Supply", 1, 8),
    ("Hash", "elems", 1, 9),
    ("Hash", "keys", 1, 8),
    ("Hash", "values", 1, 8),
    ("Hash", "kv", 1, 8),
    ("Hash", "pairs", 1, 8),
    ("Hash", "antipairs", 1, 8),
    ("Hash", "push", 8, 10),
    ("Hash", "append", 8, 10),
    ("Hash", "classify-list", 8, 12),
    ("Hash", "categorize-list", 8, 12),
    ("Hash", "Bool", 1, 9),
    ("Hash", "Str", 3, 9),
    ("Hash", "gist", 1, 9),
    ("Hash", "raku", 1, 9),
    ("Hash", "Numeric", 1, 8),
    ("Hash", "Int", 1, 8),
    ("Bool", "pred", 1, 9),
    ("Bool", "succ", 1, 9),
    ("Bool", "pick", 3, 9),
    ("Bool", "roll", 3, 9),
    ("Bool", "Numeric", 1, 8),
    ("Bool", "Int", 1, 8),
    ("Bool", "Num", 1, 8),
    ("Bool", "Rat", 3, 9),
    ("Bool", "Bool", 1, 9),
    ("Bool", "Str", 3, 9),
    ("Bool", "gist", 1, 9),
    ("Bool", "raku", 1, 9),
    ("Range", "min", 1, 8),
    ("Range", "max", 1, 8),
    ("Range", "bounds", 1, 8),
    ("Range", "elems", 1, 9),
    ("Range", "list", 1, 9),
    ("Range", "flat", 7, 9),
    ("Range", "reverse", 1, 8),
    ("Range", "pick", 3, 9),
    ("Range", "roll", 3, 9),
    ("Range", "sum", 1, 8),
    ("Range", "rand", 1, 8),
    ("Range", "minmax", 1, 8),
    ("Range", "infinite", 1, 8),
    ("Range", "is-int", 1, 8),
    ("Range", "Bool", 1, 9),
    ("Range", "Str", 3, 9),
    ("Range", "gist", 1, 9),
    ("Range", "raku", 1, 9),
    ("Range", "Numeric", 1, 8),
    ("Range", "Int", 1, 8),
    ("Range", "excludes-min", 1, 8),
    ("Range", "excludes-max", 1, 8),
    ("Blob", "allocate", 8, 12),
    ("Blob", "new", 8, 10),
    ("Blob", "push", 8, 10),
    ("Blob", "pop", 8, 10),
    ("Blob", "shift", 8, 10),
    ("Blob", "unshift", 8, 10),
    ("Blob", "append", 8, 10),
    ("Blob", "prepend", 8, 10),
    ("Blob", "splice", 8, 10),
    ("Blob", "reallocate", 8, 12),
    ("Blob", "subbuf", 6, 8),
    ("Blob", "subbuf-rw", 4, 8),
    // `decode` is recognized at both A0 (default encoding) and A1 (explicit
    // encoding name, `dispatch_1arg.rs`) -- the A1 arm needs a real encoding
    // name (`"utf-8"`), not the generic empty-string dummy
    // `native_method_arities` tries, so it was originally under-counted as
    // A0-only. Confirmed with a direct `native_method_1arg` probe,
    // 2026-08-10.
    ("Blob", "decode", 3, 8),
    ("Blob", "elems", 1, 8),
    ("Blob", "bytes", 1, 8),
    ("Blob", "of", 1, 8),
    ("Blob", "reverse", 1, 8),
    ("Blob", "list", 1, 8),
    ("Blob", "Blob", 1, 8),
    ("Blob", "Buf", 1, 8),
    ("Blob", "Bool", 1, 8),
    ("Blob", "Str", 3, 8),
    ("Blob", "gist", 1, 8),
    ("Blob", "raku", 1, 8),
    // ADR-0019 E2b (sixth slice, 2026-08-10): `Blob`/`Buf`-family extra rows
    // found once the `record_native_row_coverage` `canonical_builtin_owner`
    // fold (`receiver_class.rs`) started routing `Buf`/`utf8`/`Buf[uint8]`
    // receivers to this table's `Blob` rows -- `values`/`List`/the
    // `read-*`/`write-*` native-endian accessor family
    // (`dispatch_1arg.rs`) were never probed by the original 11-owner E2a
    // generation pass (`Blob` was not one of the 11). `read-*` is A1
    // (offset) or A2 (offset, endianness), never A0.
    ("Blob", "values", 1, 0),
    ("Blob", "List", 1, 0),
    ("Blob", "read-uint8", 6, 8),
    ("Blob", "read-int8", 6, 8),
    ("Blob", "read-uint16", 6, 8),
    ("Blob", "read-int16", 6, 8),
    ("Blob", "read-uint32", 6, 8),
    // `.DEFINITE` is a quoted pseudo-method (like `.WHAT`/`.HOW`/`.WHICH`),
    // deliberately excluded from `MU_METHODS`'s `.^methods` introspection
    // list since it is a compiler-level construct rather than an ordinary
    // dispatchable method -- but `dispatch_core_coerce::dispatch` still
    // recognizes it via the plain arity-0 cascade for any receiver, so it
    // needs a SPECIAL row (never claims a `.^methods`-visible slot) to stop
    // being counted as unmodeled.
    ("Mu", "DEFINITE", 1, 12),
    // ADR-0019 E2b: `Pair`/`Seq` rows, hand-probed against a real
    // `Value::pair`/`Value::seq` sample (2026-08-10) -- neither owner has a
    // `builtin_type_method_names` entry to draw candidates from (see
    // `builtin_type_method_names`'s `_ => Vec::new()` default), so the
    // candidate list was curated from the `Seq`/`Pair` type docs plus the
    // `native_call_unmodeled` sweep breakdown instead of the usual
    // probe-every-catalog-name pass. `so`/`not`/`defined` are deliberately
    // absent here: they are already covered by the `Any` rows above via the
    // chain-walk in `Interpreter::record_native_row_coverage`. Verified by
    // `pair_seq_rows_are_backed_by_the_cascade` in `native_method_row.rs`.
    ("Pair", "antipair", 1, 0),
    ("Pair", "Array", 1, 0),
    ("Pair", "batch", 2, 0),
    ("Pair", "Bool", 1, 0),
    ("Pair", "cache", 1, 0),
    ("Pair", "elems", 1, 0),
    ("Pair", "end", 1, 0),
    ("Pair", "flat", 7, 0),
    ("Pair", "gist", 1, 0),
    ("Pair", "head", 3, 0),
    ("Pair", "invert", 1, 0),
    ("Pair", "item", 1, 0),
    ("Pair", "join", 3, 0),
    ("Pair", "key", 1, 0),
    ("Pair", "keys", 1, 0),
    ("Pair", "kv", 1, 0),
    ("Pair", "lazy", 1, 0),
    ("Pair", "list", 1, 0),
    ("Pair", "List", 1, 0),
    ("Pair", "max", 1, 0),
    ("Pair", "min", 1, 0),
    ("Pair", "pairs", 1, 0),
    ("Pair", "pick", 3, 0),
    ("Pair", "raku", 1, 0),
    ("Pair", "reverse", 1, 0),
    ("Pair", "roll", 3, 0),
    ("Pair", "sink", 1, 0),
    ("Pair", "Slip", 1, 0),
    ("Pair", "Str", 3, 0),
    ("Pair", "tail", 3, 0),
    ("Pair", "unique", 1, 0),
    ("Pair", "value", 1, 0),
    ("Pair", "values", 1, 0),
    ("Pair", "WHICH", 1, 0),
    ("Seq", "Array", 1, 0),
    ("Seq", "batch", 2, 0),
    ("Seq", "Bool", 1, 0),
    ("Seq", "cache", 1, 0),
    ("Seq", "elems", 1, 0),
    ("Seq", "end", 1, 0),
    ("Seq", "flat", 7, 0),
    ("Seq", "gist", 1, 0),
    ("Seq", "head", 3, 0),
    ("Seq", "invert", 1, 0),
    ("Seq", "item", 1, 0),
    ("Seq", "join", 3, 0),
    ("Seq", "keys", 1, 0),
    ("Seq", "kv", 1, 0),
    ("Seq", "lazy", 1, 0),
    ("Seq", "list", 1, 0),
    ("Seq", "List", 1, 0),
    ("Seq", "pairs", 1, 0),
    ("Seq", "pick", 3, 0),
    ("Seq", "raku", 1, 0),
    ("Seq", "reverse", 1, 0),
    ("Seq", "roll", 3, 0),
    ("Seq", "sink", 1, 0),
    ("Seq", "Slip", 1, 0),
    ("Seq", "Str", 3, 0),
    ("Seq", "tail", 3, 0),
    ("Seq", "unique", 1, 0),
    ("Seq", "values", 1, 0),
    ("Seq", "WHICH", 1, 0),
    // ADR-0019 E2b: `Match` rows, hand-probed against a real Match value
    // produced by running `'foo' ~~ /f(o)(o)/` through the interpreter
    // (2026-08-10) -- like `Pair`/`Seq`, `Match` has no
    // `builtin_type_method_names` entry to draw candidates from. Two
    // candidate sources were probed: the explicit 0-arg arm in
    // `methods_0arg/mod.rs` (`"from" | "to" | "pos" | ...`), and every `Str`
    // row name above -- that arm's `_` default falls through to
    // `native_method_0arg` on the matched string
    // (`Value::str(target.to_string_value())`), and the narg cascades for
    // string-shaped methods (`split`/`substr`/`comb`/...) coerce via
    // `target.to_string_value()` regardless of receiver type, so most of
    // `Str`'s surface is reachable from a `Match` receiver too. Only names
    // the probe actually recognized (non-zero arity bits) are listed here --
    // `replace-with`/`ends-with`/`indices`/`match`/`starts-with`/`subst`/
    // `subst-mutate`/`substr-rw`/`substr-eq`/`trans`/`IO` returned zero and
    // are deliberately absent (not natively recognized for a Match
    // receiver). `so`/`not`/`defined` are deliberately absent here too: a
    // Match's `dispatch_owner_chain` includes `Any`, so they are already
    // covered by the `Any` rows above via the chain-walk in
    // `Interpreter::record_native_row_coverage`, same as `Pair`/`Seq`.
    // Verified by `match_rows_are_backed_by_the_cascade` in
    // `native_method_row.rs`.
    ("Match", "pos", 1, 0),
    ("Match", "target", 1, 0),
    ("Match", "clone", 1, 0),
    ("Match", "orig", 1, 0),
    ("Match", "from", 1, 0),
    ("Match", "to", 1, 0),
    ("Match", "made", 1, 0),
    ("Match", "actions", 1, 0),
    ("Match", "ast", 1, 0),
    ("Match", "Bool", 1, 0),
    ("Match", "Numeric", 1, 0),
    ("Match", "caps", 1, 0),
    ("Match", "chunks", 1, 0),
    ("Match", "list", 1, 0),
    ("Match", "hash", 1, 0),
    ("Match", "Hash", 1, 0),
    ("Match", "Array", 1, 0),
    ("Match", "prematch", 1, 0),
    ("Match", "postmatch", 1, 0),
    ("Match", "perl", 1, 0),
    ("Match", "WHICH", 1, 0),
    ("Match", "keys", 1, 0),
    ("Match", "values", 1, 0),
    ("Match", "pairs", 1, 0),
    ("Match", "kv", 1, 0),
    ("Match", "elems", 1, 0),
    ("Match", "Capture", 1, 0),
    ("Match", "chars", 1, 0),
    ("Match", "codes", 1, 0),
    ("Match", "comb", 7, 0),
    ("Match", "chomp", 1, 0),
    ("Match", "chop", 3, 0),
    ("Match", "contains", 2, 0),
    ("Match", "fc", 1, 0),
    ("Match", "flip", 1, 0),
    ("Match", "index", 2, 0),
    ("Match", "lc", 1, 0),
    ("Match", "lines", 3, 0),
    ("Match", "ords", 1, 0),
    ("Match", "pred", 1, 0),
    ("Match", "rindex", 2, 0),
    ("Match", "samecase", 2, 0),
    ("Match", "samemark", 2, 0),
    ("Match", "split", 6, 0),
    ("Match", "substr", 6, 0),
    ("Match", "succ", 1, 0),
    ("Match", "tc", 1, 0),
    ("Match", "trim", 1, 0),
    ("Match", "trim-leading", 1, 0),
    ("Match", "trim-trailing", 1, 0),
    ("Match", "uc", 1, 0),
    ("Match", "words", 3, 0),
    ("Match", "wordcase", 1, 0),
    ("Match", "NFC", 1, 0),
    ("Match", "NFD", 1, 0),
    ("Match", "NFKC", 1, 0),
    ("Match", "NFKD", 1, 0),
    ("Match", "encode", 1, 0),
    ("Match", "uniparse", 1, 0),
    ("Match", "unimatch", 6, 0),
    ("Match", "uniprops", 3, 0),
    ("Match", "parse-names", 1, 0),
    ("Match", "parse-base", 2, 0),
    ("Match", "Int", 1, 0),
    ("Match", "Num", 1, 0),
    ("Match", "Rat", 3, 0),
    ("Match", "Str", 3, 0),
    ("Match", "gist", 1, 0),
    ("Match", "raku", 1, 0),
    ("Match", "fmt", 7, 0),
    // ADR-0019 E2b (fifth slice, 2026-08-10): seven more `Any` universal
    // pseudo-methods, alongside the existing `so`/`not`/`defined`/`DEFINITE`
    // rows above -- `self`/`clone`/`WHERE`/`WHICH`/`sink`/`item`/`serial`
    // each have a receiver-type-agnostic `_ => ...` fallback arm in
    // `dispatch_core_coerce.rs` (`self`/`WHERE`/`WHICH`/`serial`/`clone`) or
    // `dispatch_core_math.rs` (`sink`/`item`), found by reading every match
    // arm in those two files rather than guessing from the sweep breakdown
    // alone. Verified by
    // `any_second_batch_universal_rows_are_backed_by_the_cascade` in
    // `native_method_row.rs`.
    ("Any", "self", 1, 0),
    ("Any", "clone", 1, 0),
    ("Any", "WHICH", 1, 0),
    ("Any", "sink", 1, 0),
    ("Any", "item", 1, 0),
    ("Any", "serial", 1, 8),
    // ADR-0019 E2b (fifth slice): `Str` extra rows, hand-probed against a
    // real `Value::str_from("abc")` sample -- the Unicode-method cluster
    // (`ord`/`uniname`/`uninames`/`unival`/`univals`/`chrs`/`bytes`) lives in
    // `dispatch_core_unicode.rs` alongside the already-modeled `uniprops`;
    // `uniprop` (singular; distinct from the plural `uniprops` row above) is
    // in the same file. `AST` is `Str`-only (`methods_0arg/mod.rs`, parses
    // the string as Raku source, ADR-0010). `indent` is 1-arg
    // (`methods_narg/dispatch_1arg.rs`). `sprintf`'s recognition depends on
    // the receiver's own content (needs exactly one `%`-directive), not just
    // arg shape -- see the dedicated probe sample in
    // `fifth_slice_extra_rows_are_backed_by_the_cascade`. The remaining
    // names (`list`/`UInt`/`FatRat`/`tclc`/`Range`/`Complex`/`Version`/
    // `Real`/`Date`/`DateTime`/`reverse`/`byte`/`perl`) are ordinary coercion
    // arms that also happen to fire for a plain `Str` receiver. Verified by
    // `fifth_slice_extra_rows_are_backed_by_the_cascade` in
    // `native_method_row.rs`.
    ("Str", "uniprop", 3, 8),
    ("Str", "AST", 1, 0),
    ("Str", "indent", 2, 8),
    ("Str", "list", 1, 0),
    ("Str", "UInt", 1, 0),
    ("Str", "FatRat", 3, 0),
    ("Str", "sprintf", 2, 0),
    ("Str", "ord", 1, 8),
    ("Str", "uniname", 1, 8),
    ("Str", "uninames", 1, 8),
    ("Str", "unival", 1, 8),
    ("Str", "univals", 1, 8),
    ("Str", "chrs", 1, 0),
    ("Str", "bytes", 1, 0),
    ("Str", "tclc", 1, 8),
    ("Str", "Range", 1, 0),
    ("Str", "Complex", 1, 0),
    ("Str", "Version", 1, 8),
    ("Str", "Real", 1, 0),
    ("Str", "Date", 1, 8),
    ("Str", "DateTime", 1, 8),
    ("Str", "reverse", 1, 0),
    ("Str", "byte", 1, 0),
    ("Str", "perl", 1, 0),
    // ADR-0019 E2b (fifth slice): `Hash` extra rows, hand-probed against a
    // real `Value::hash(...)` sample. `pick`/`roll` (1-arg count form, plus
    // a bare 0-arg single-pick) live in `dispatch_1arg.rs`/
    // `dispatch_core_range.rs`. `EXISTS-KEY`/`AT-KEY`/`AT-POS`/
    // `EXISTS-POS` are the postcircumfix-subscript protocol methods
    // (`dispatch_1arg.rs`). `List`/`Array`/`invert`/`flat`/`dynamic`/`perl`
    // are ordinary coercion/collection arms that also fire for a Hash
    // receiver. Verified by
    // `fifth_slice_extra_rows_are_backed_by_the_cascade` in
    // `native_method_row.rs`.
    ("Hash", "pick", 3, 8),
    ("Hash", "EXISTS-KEY", 2, 8),
    ("Hash", "AT-KEY", 2, 8),
    ("Hash", "List", 1, 8),
    ("Hash", "invert", 1, 8),
    ("Hash", "flat", 7, 8),
    ("Hash", "Array", 1, 0),
    ("Hash", "AT-POS", 2, 0),
    ("Hash", "EXISTS-POS", 2, 0),
    ("Hash", "dynamic", 1, 8),
    ("Hash", "roll", 3, 8),
    ("Hash", "perl", 1, 0),
    // ADR-0019 E2b (fifth slice): `Int` extra rows, hand-probed against a
    // real `Value::int(2)` sample. `rand`/`elems`/`lsb`/`msb` live in the
    // shared numeric-method cluster `dispatch_core_numeric.rs`, which is
    // tried for every receiver by name only (not gated on a numeric
    // `ValueView`) -- `elems`/`rand` end up recognized for a bare `Int` the
    // same way `uc`/`lc`/`fc`/`tc` do for a bare `Str` (already modeled).
    // `flip`/`uniprop`/`uc` reach an `Int` receiver the same way: their
    // dispatch arms call `target.to_string_value()` unconditionally.
    // `numerator`/`denominator`/`UInt`/`Real`/`Version`/`Complex`/`perl`/
    // `reverse`/`kv`/`pairs`/`int8`/`wordcase`/`rindex`/`EXISTS-KEY`/
    // `AT-KEY`/`EXISTS-POS`/`Array`/`Supply`/`fmt` are ordinary
    // coercion/collection/subscript arms that also fire for a plain `Int`
    // receiver (real Raku semantics for some of these are a separate
    // question from whether mutsu's cascade recognizes the name -- E2b only
    // models the latter). Verified by
    // `fifth_slice_extra_rows_are_backed_by_the_cascade` in
    // `native_method_row.rs`.
    ("Int", "rand", 1, 8),
    ("Int", "elems", 1, 0),
    ("Int", "flip", 1, 0),
    ("Int", "uniprop", 3, 8),
    ("Int", "fmt", 7, 0),
    ("Int", "EXISTS-POS", 2, 0),
    ("Int", "Array", 1, 0),
    ("Int", "lsb", 1, 8),
    ("Int", "msb", 1, 8),
    ("Int", "Supply", 1, 0),
    ("Int", "pairs", 1, 0),
    ("Int", "denominator", 1, 0),
    ("Int", "numerator", 1, 0),
    ("Int", "kv", 1, 0),
    ("Int", "int8", 1, 8),
    ("Int", "rindex", 2, 0),
    ("Int", "EXISTS-KEY", 2, 0),
    ("Int", "AT-KEY", 2, 0),
    ("Int", "UInt", 1, 0),
    ("Int", "wordcase", 1, 0),
    ("Int", "uc", 1, 0),
    ("Int", "Real", 1, 8),
    ("Int", "Version", 1, 0),
    ("Int", "Complex", 1, 8),
    ("Int", "perl", 1, 0),
    ("Int", "reverse", 1, 0),
    // ADR-0019 E2b (sixth slice, 2026-08-10): `Set`/`SetHash`/`Bag`/
    // `BagHash`/`Mix`/`MixHash` rows, hand-probed against real values built
    // via `set(...)`/`SetHash.new(...)`/etc through the interpreter --
    // none of the six owners has a `builtin_type_method_names` entry, same
    // situation as `Pair`/`Seq`/`Match`. `grab` (weighted removal) is
    // deliberately absent from `Set`/`SetHash`: those carry no weights, and
    // the cascade genuinely does not recognize it there (confirmed by
    // `setbagmix_rows_are_backed_by_the_cascade` in `native_method_row.rs`,
    // which also probes the `Set`/`SetHash` omission directly).
    ("Set", "keys", 1, 0),
    ("Set", "values", 1, 0),
    ("Set", "kv", 1, 0),
    ("Set", "pairs", 1, 0),
    ("Set", "elems", 1, 0),
    ("Set", "gist", 1, 0),
    ("Set", "raku", 1, 0),
    ("Set", "Str", 3, 0),
    ("Set", "Bool", 1, 0),
    ("Set", "list", 1, 0),
    ("Set", "List", 1, 0),
    ("Set", "Array", 1, 0),
    ("Set", "total", 1, 0),
    // Immutable `Set`'s `grab` IS recognized by the pure cascade
    // (`dispatch_core_range.rs`'s `ValueView::Set(_, false)` arm) -- it
    // always errors ("Cannot call .grab on an immutable Set"), but `Some`
    // still counts as recognized. The mutable `SetHash` variant
    // (`Set(_, true)`) falls through to the slow path instead (same as
    // `BagHash`/`MixHash` above), so `SetHash` deliberately has no `grab`
    // row here (defaults to `N`/`SPECIAL`).
    ("Set", "grab", 3, 0),
    ("Set", "pick", 3, 0),
    ("Set", "roll", 3, 0),
    ("Set", "WHICH", 1, 0),
    ("SetHash", "keys", 1, 0),
    ("SetHash", "values", 1, 0),
    ("SetHash", "kv", 1, 0),
    ("SetHash", "pairs", 1, 0),
    ("SetHash", "elems", 1, 0),
    ("SetHash", "gist", 1, 0),
    ("SetHash", "raku", 1, 0),
    ("SetHash", "Str", 3, 0),
    ("SetHash", "Bool", 1, 0),
    ("SetHash", "list", 1, 0),
    ("SetHash", "List", 1, 0),
    ("SetHash", "Array", 1, 0),
    ("SetHash", "total", 1, 0),
    ("SetHash", "pick", 3, 0),
    ("SetHash", "roll", 3, 0),
    ("SetHash", "WHICH", 1, 0),
    ("Bag", "keys", 1, 0),
    ("Bag", "values", 1, 0),
    ("Bag", "kv", 1, 0),
    ("Bag", "pairs", 1, 0),
    ("Bag", "elems", 1, 0),
    ("Bag", "gist", 1, 0),
    ("Bag", "raku", 1, 0),
    ("Bag", "Str", 3, 0),
    ("Bag", "Bool", 1, 0),
    ("Bag", "list", 1, 0),
    ("Bag", "List", 1, 0),
    ("Bag", "Array", 1, 0),
    ("Bag", "total", 1, 0),
    ("Bag", "grab", 3, 0),
    ("Bag", "pick", 3, 0),
    ("Bag", "roll", 3, 0),
    ("Bag", "WHICH", 1, 0),
    ("BagHash", "keys", 1, 0),
    ("BagHash", "values", 1, 0),
    ("BagHash", "kv", 1, 0),
    ("BagHash", "pairs", 1, 0),
    ("BagHash", "elems", 1, 0),
    ("BagHash", "gist", 1, 0),
    ("BagHash", "raku", 1, 0),
    ("BagHash", "Str", 3, 0),
    ("BagHash", "Bool", 1, 0),
    ("BagHash", "list", 1, 0),
    ("BagHash", "List", 1, 0),
    ("BagHash", "Array", 1, 0),
    ("BagHash", "total", 1, 0),
    // `grab` on the *mutable* `BagHash`/`MixHash` variant is served by the
    // `&mut self` slow path (`methods_mut_dispatch.rs`), not the pure
    // arity cascade -- unlike the immutable `Bag`/`Mix`, whose `grab` the
    // cascade DOES recognize (always erroring "immutable", still `Some`).
    // Confirmed by probe: `native_method_arities` returns 0 for a `BagHash`
    // sample. SPECIAL, not omitted, to keep the choice explicit.
    ("BagHash", "grab", 8, 4),
    ("BagHash", "pick", 3, 0),
    ("BagHash", "roll", 3, 0),
    ("BagHash", "WHICH", 1, 0),
    ("Mix", "keys", 1, 0),
    ("Mix", "values", 1, 0),
    ("Mix", "kv", 1, 0),
    ("Mix", "pairs", 1, 0),
    ("Mix", "elems", 1, 0),
    ("Mix", "gist", 1, 0),
    ("Mix", "raku", 1, 0),
    ("Mix", "Str", 3, 0),
    ("Mix", "Bool", 1, 0),
    ("Mix", "list", 1, 0),
    ("Mix", "List", 1, 0),
    ("Mix", "Array", 1, 0),
    ("Mix", "total", 1, 0),
    ("Mix", "grab", 3, 0),
    ("Mix", "pick", 3, 0),
    ("Mix", "roll", 3, 0),
    ("Mix", "WHICH", 1, 0),
    ("MixHash", "keys", 1, 0),
    ("MixHash", "values", 1, 0),
    ("MixHash", "kv", 1, 0),
    ("MixHash", "pairs", 1, 0),
    ("MixHash", "elems", 1, 0),
    ("MixHash", "gist", 1, 0),
    ("MixHash", "raku", 1, 0),
    ("MixHash", "Str", 3, 0),
    ("MixHash", "Bool", 1, 0),
    ("MixHash", "list", 1, 0),
    ("MixHash", "List", 1, 0),
    ("MixHash", "Array", 1, 0),
    ("MixHash", "total", 1, 0),
    // Same as `BagHash`'s `grab` above: the mutable `MixHash` variant's
    // `grab` is slow-path-only, not pure-cascade-recognized.
    ("MixHash", "grab", 8, 4),
    ("MixHash", "pick", 3, 0),
    ("MixHash", "roll", 3, 0),
    ("MixHash", "WHICH", 1, 0),
    // ADR-0019 E2b (sixth slice): `RakuAST::StatementList` rows, hand-probed
    // against a real `Str.AST` parse tree (`'my $x = 1 + 2;'.AST`) -- no
    // `builtin_type_method_names` entry either. `RakuAST::Statement::Expression`'s
    // `expression` field accessor reaches the generic `rakuast::node_accessor`
    // dispatch (`methods_0arg/mod.rs`), the same mechanism every RakuAST node
    // class uses for its own fields, not something `StatementList`-specific.
    // Verified by `rakuast_statementlist_rows_are_backed_by_the_cascade` in
    // `native_method_row.rs`.
    ("RakuAST::StatementList", "gist", 1, 0),
    ("RakuAST::StatementList", "statements", 1, 0),
    ("RakuAST::StatementList", "add-statement", 2, 0),
    ("RakuAST::StatementList", "raku", 1, 0),
    ("RakuAST::StatementList", "Str", 3, 0),
    ("RakuAST::StatementList", "WHICH", 1, 0),
    ("RakuAST::StatementList", "list", 1, 0),
    ("RakuAST::StatementList", "List", 1, 0),
    ("RakuAST::StatementList", "elems", 1, 0),
    ("RakuAST::StatementList", "Bool", 1, 0),
    ("RakuAST::StatementList", "flat", 7, 0),
    ("RakuAST::Statement::Expression", "expression", 1, 0),
    // ADR-0019 E2b (seventh slice, 2026-08-10): `Failure`/`X::AdHoc`/
    // `CX::Warn`/`X::TypeCheck::Assignment` rows, hand-probed against real
    // values raised via the interpreter -- `so`/`not`/`defined`/`self`/
    // `clone`/`WHERE`/`WHICH`/`sink`/`item`/`serial` are deliberately absent
    // from all four: they are the `Any` universal rows above, now reachable
    // via the chain walk after the `Exception`/`CX::Warn`/`Failure`
    // `builtin_type_catalog` fixes. `resume`/`backtrace`/`message`/`throw`/
    // `raku` vary per concrete type (confirmed by direct probe, not assumed
    // shared -- e.g. `CX::Warn` lacks `throw`/`raku`; `Failure` lacks
    // `message`/`backtrace`). Verified by
    // `exception_family_rows_are_backed_by_the_cascade` in
    // `native_method_row.rs`.
    ("Failure", "resume", 1, 0),
    ("Failure", "exception", 1, 0),
    ("Failure", "handled", 1, 0),
    ("Failure", "gist", 1, 0),
    ("Failure", "raku", 1, 0),
    ("Failure", "Str", 3, 0),
    ("Failure", "Bool", 1, 0),
    ("Failure", "throw", 1, 0),
    ("X::AdHoc", "message", 1, 0),
    ("X::AdHoc", "resume", 1, 0),
    ("X::AdHoc", "backtrace", 1, 0),
    ("X::AdHoc", "gist", 1, 0),
    ("X::AdHoc", "Str", 3, 0),
    ("X::AdHoc", "Bool", 1, 0),
    ("X::AdHoc", "throw", 1, 0),
    ("CX::Warn", "message", 1, 0),
    ("CX::Warn", "resume", 1, 0),
    ("CX::Warn", "backtrace", 1, 0),
    ("CX::Warn", "gist", 1, 0),
    ("CX::Warn", "Str", 3, 0),
    ("CX::Warn", "Bool", 1, 0),
    ("X::TypeCheck::Assignment", "message", 1, 0),
    ("X::TypeCheck::Assignment", "resume", 1, 0),
    ("X::TypeCheck::Assignment", "backtrace", 1, 0),
    ("X::TypeCheck::Assignment", "gist", 1, 0),
    ("X::TypeCheck::Assignment", "Str", 3, 0),
    ("X::TypeCheck::Assignment", "Bool", 1, 0),
    ("X::TypeCheck::Assignment", "throw", 1, 0),
    // ADR-0019 E2b (eighth slice, 2026-08-10): the sweep's remaining tail was a
    // long list of single-digit-to-dozens owners with no dominant offender.
    // Each row below was hand-probed against a real value of that owner
    // (constructed via the interpreter, not `builtin_sample_value`, which has
    // no entry for any of these) using `native_method_arities`, the same
    // discipline as every earlier E2b slice. Two are root-cause fixes rather
    // than plain additions:
    // - `Any`'s `gist`/`raku`/`hash` cover the BARE `Any` type object
    //   (`Any.gist` -> "(Any)"), reached via the generic `ValueView::Package`
    //   formatting arm in `dispatch_core_repr.rs` that renders every type
    //   object uniformly -- confirmed the same arm serves user classes too
    //   (`class Foo {}; Foo.gist` -> "(Foo)"), so the row is not a
    //   `Any`-sample artifact.
    // - `Exception`'s `message`/`gist`/`Str` are declared at the shared
    //   `cn == "Exception" || cn.starts_with("X::") || cn.starts_with("CX::")`
    //   gate in `methods_0arg/mod.rs`, so one `Exception`-owner row (found via
    //   the chain-walk, same as `Failure`'s fix in the seventh slice) covers
    //   every `X::*`/`CX::*` type that does NOT already have its own
    //   more-specific row above -- verified against three previously-unmodeled
    //   types (`X::Method::NotFound`, `X::Str::Sprintf::Directives::Unsupported`,
    //   `X::Str::Numeric`) without adding a row for any of them individually.
    ("Any", "hash", 1, 8),
    ("Mu", "defined", 1, 8),
    ("Nil", "gist", 1, 0),
    ("Nil", "raku", 1, 0),
    ("Exception", "message", 1, 0),
    ("Exception", "gist", 1, 0),
    ("Exception", "Str", 3, 0),
    // ADR-0019 E2b (tenth slice): `line`/`file`/`backtrace` and `throw`/
    // `resume` are declared in the SAME `cn == "Exception" ||
    // cn.starts_with("X::") || cn.starts_with("CX::")`-gated match blocks in
    // `methods_0arg/mod.rs` as `message`/`gist`/`Str` above (the `throw`/
    // `resume` gates additionally admit `Failure` and `CX::Warn` by name, but
    // those already have their own rows), so one `Exception`-owner row each
    // covers every `X::*`/`CX::*` subtype the chain walk now reaches (see the
    // `register_x` additions in `runtime_init.rs` this same slice).
    ("Exception", "line", 1, 0),
    ("Exception", "file", 1, 0),
    ("Exception", "backtrace", 1, 0),
    ("Exception", "throw", 1, 0),
    ("Exception", "resume", 1, 0),
    ("Version", "Str", 3, 0),
    ("Version", "raku", 1, 0),
    ("Version", "gist", 1, 0),
    ("Version", "parts", 1, 0),
    ("Date", "Str", 3, 0),
    ("Date", "raku", 1, 0),
    ("Date", "gist", 1, 0),
    ("Date", "year", 1, 0),
    ("Date", "mm-dd-yyyy", 1, 0),
    ("Date", "yyyy-mm-dd", 1, 0),
    ("Date", "dd-mm-yyyy", 1, 0),
    ("DateTime", "Str", 3, 0),
    ("DateTime", "raku", 1, 0),
    ("DateTime", "gist", 1, 0),
    ("DateTime", "hour", 1, 0),
    ("DateTime", "year", 1, 0),
    ("Duration", "Numeric", 1, 0),
    ("Duration", "abs", 1, 0),
    ("Duration", "Int", 1, 0),
    ("Duration", "gist", 1, 0),
    ("Duration", "raku", 1, 0),
    ("Backtrace", "list", 1, 0),
    ("Backtrace", "Str", 3, 0),
    ("Backtrace", "gist", 1, 0),
    ("Backtrace::Frame", "is-routine", 1, 0),
    ("Backtrace::Frame", "subname", 1, 0),
    ("Backtrace::Frame", "is-hidden", 1, 0),
    ("Backtrace::Frame", "file", 1, 0),
    ("Backtrace::Frame", "line", 1, 0),
    ("Backtrace::Frame", "gist", 1, 0),
    ("Range", "hyper", 1, 8),
    ("Range", "lazy", 1, 8),
    ("Range", "int-bounds", 1, 8),
    ("Range", "Array", 1, 0),
    ("Range", "join", 1, 0),
    ("Range", "AT-POS", 2, 8),
    ("Range", "Supply", 1, 0),
    ("Range", "race", 1, 8),
    ("Range", "List", 1, 0),
    ("Range", "in-range", 2, 8),
    ("Range", "head", 3, 0),
    ("Range", "EXISTS-POS", 2, 8),
    ("Range", "batch", 2, 0),
    ("Rat", "FatRat", 3, 8),
    ("Rat", "nude", 1, 8),
    ("Map", "raku", 1, 0),
    ("Map", "gist", 1, 0),
    ("Map", "keys", 1, 0),
    ("Map", "elems", 1, 0),
    ("Pair", "Pair", 1, 0),
    ("CallFrame", "defined", 1, 0),
    ("List", "tree", 1, 0),
    ("List", "pairup", 1, 0),
    ("List", "hash", 1, 0),
    ("List", "fmt", 7, 8),
    ("Array", "tree", 1, 0),
    ("Array", "pairup", 1, 0),
    ("Array", "hash", 1, 0),
    ("Array", "fmt", 7, 8),
    ("Attribute", "defined", 1, 0),
    ("IO::Path::Parts", "AT-KEY", 2, 0),
    ("IO::Path::Parts", "AT-POS", 2, 0),
    ("Capture", "list", 1, 0),
    ("Capture", "hash", 1, 0),
    ("Complex", "UInt", 1, 0),
    ("Complex", "isNaN", 1, 8),
    ("Instant", "to-posix", 1, 0),
    ("Instant", "Numeric", 1, 0),
    ("Uni", "Str", 3, 0),
    ("Block", "lazy", 1, 0),
    ("Supply", "list", 1, 0),
    ("Junction", "gist", 1, 0),
    ("Junction", "Bool", 1, 0),
    ("Seq", "is-lazy", 1, 0),
    ("Match", "Stringy", 1, 0),
    ("Match", "join", 3, 0),
    ("Match", "AT-POS", 2, 0),
    // ADR-0019 E2b (ninth slice, 2026-08-10): three more coherent clusters
    // hand-probed the same way -- the full `Date`/`DateTime` accessor
    // surface and `Backtrace`/`Backtrace::Frame`/`Complex` extras left after
    // the eighth slice's partial coverage. The remaining sweep tail after
    // this slice is genuinely one-off (RakuAST node accessors, NativeCall
    // `CArray[T]` variants, ad-hoc test-fixture class names) with no
    // reusable owner cluster left -- see the ADR progress note for why
    // chasing it further has diminishing returns.
    ("Date", "day", 1, 0),
    ("Date", "month", 1, 0),
    ("Date", "formatter", 1, 0),
    ("Date", "day-of-week", 1, 0),
    ("Date", "succ", 1, 0),
    ("Date", "perl", 1, 0),
    ("Date", "days-in-year", 1, 0),
    ("Date", "daycount", 1, 0),
    ("DateTime", "minute", 1, 0),
    ("DateTime", "Date", 1, 0),
    ("DateTime", "offset-in-minutes", 1, 0),
    ("DateTime", "day", 1, 0),
    ("DateTime", "month", 1, 0),
    ("DateTime", "second", 1, 0),
    ("DateTime", "timezone", 1, 0),
    ("DateTime", "days-in-year", 1, 0),
    ("DateTime", "dd-mm-yyyy", 1, 0),
    ("DateTime", "mm-dd-yyyy", 1, 0),
    ("DateTime", "yyyy-mm-dd", 1, 0),
    ("DateTime", "offset-in-hours", 1, 0),
    ("DateTime", "Instant", 1, 0),
    ("Backtrace", "flat", 7, 0),
    ("Backtrace", "defined", 1, 0),
    ("Backtrace", "concise", 1, 0),
    ("Backtrace", "summary", 1, 0),
    ("Backtrace", "Stringy", 1, 0),
    ("Backtrace::Frame", "is-setting", 1, 0),
    ("Backtrace::Frame", "code", 1, 0),
    ("Backtrace::Frame", "Str", 3, 0),
    ("Complex", "re", 1, 8),
    ("Complex", "im", 1, 8),
    ("Complex", "reals", 1, 8),
    ("Complex", "conj", 1, 8),
    ("Complex", "reverse", 1, 0),
    ("Complex", "Complex", 1, 8),
    // ADR-0019 E2b (eleventh slice, 2026-08-10): the RakuAST node-accessor
    // family, the largest remaining homogeneous cluster after the tenth
    // slice closed `X::*`. Every field-accessor call on a `RakuAST::*` node
    // (`rakuast::node_accessor`, `methods_0arg/mod.rs`) is served by ONE
    // shared, data-driven 0-arg dispatch site that reads the node's own
    // `fields` list by name -- there is no per-class Rust match arm to point
    // a comment at, so each row here is simply "this class's real field
    // list", hand-probed against live nodes built the same two ways the
    // existing `t/rakuast-construct-*.t` suite already does: direct
    // `RakuAST::Foo.new(...)` construction for `Parameter`/
    // `ParameterTarget::Var`/`Type::Simple`/`StrLiteral`, and `Q[...].AST`
    // deparse for everything reached more naturally by parsing (a plain
    // string literal like `"abc"` deparses to `QuotedString`, not
    // `StrLiteral` -- confirmed by direct probe, not assumed). See
    // `rakuast::accessor_names` for the (separate, and in a few cases
    // incomplete -- e.g. it does not list `QuotedString`/`Call::Name::
    // WithoutParentheses`/`Statement::If`/`PointyBlock`) introspection-only
    // registry `.^methods`/`.^attributes` read; `node_accessor`'s real
    // per-instance `fields` list is the actual source of truth these rows
    // were verified against.
    ("RakuAST::IntLiteral", "value", 1, 0),
    ("RakuAST::RatLiteral", "value", 1, 0),
    ("RakuAST::StrLiteral", "value", 1, 0),
    ("RakuAST::QuotedString", "segments", 1, 0),
    ("RakuAST::Var::Lexical", "name", 1, 0),
    ("RakuAST::VarDeclaration::Simple", "sigil", 1, 0),
    ("RakuAST::VarDeclaration::Simple", "desigilname", 1, 0),
    ("RakuAST::VarDeclaration::Simple", "initializer", 1, 0),
    ("RakuAST::Initializer::Assign", "expression", 1, 0),
    ("RakuAST::ApplyInfix", "left", 1, 0),
    ("RakuAST::ApplyInfix", "right", 1, 0),
    ("RakuAST::ApplyPrefix", "operand", 1, 0),
    ("RakuAST::Call::Name::WithoutParentheses", "name", 1, 0),
    ("RakuAST::Statement::If", "condition", 1, 0),
    ("RakuAST::Statement::If", "then", 1, 0),
    ("RakuAST::Block", "body", 1, 0),
    ("RakuAST::Blockoid", "statement-list", 1, 0),
    ("RakuAST::PointyBlock", "signature", 1, 0),
    ("RakuAST::Sub", "name", 1, 0),
    ("RakuAST::Sub", "signature", 1, 0),
    ("RakuAST::Sub", "body", 1, 0),
    ("RakuAST::Signature", "parameters", 1, 0),
    ("RakuAST::Parameter", "type", 1, 0),
    ("RakuAST::Parameter", "names", 1, 0),
    ("RakuAST::Parameter", "target", 1, 0),
    ("RakuAST::Parameter", "optional", 1, 0),
    ("RakuAST::Parameter", "default", 1, 0),
    ("RakuAST::Parameter", "where", 1, 0),
    ("RakuAST::Parameter", "slurpy", 1, 0),
    ("RakuAST::ParameterTarget::Var", "name", 1, 0),
    ("RakuAST::Type::Simple", "name", 1, 0),
    // ADR-0019 E11 slice 2 (2026-08-14): rows for the seven owners E2a's
    // original generation pass never covered -- `builtin_sample_value` (the
    // sample-value probe `#[cfg(test)]` helper) had no branch for `Cool`
    // (abstract, no concrete instance), `Any`/`Mu` beyond the handful E2b
    // added by hand, or `Sub`/`Signature`/`IO::Path`/`IO::Handle` (need a
    // real interpreter-constructed value, not a bare `Value::` literal) --
    // see the E11 progress note in the ADR for the gap this closes (E7 step
    // 4's `.^can` shadow-check found `real=true shadow=false` mismatches for
    // any name only reachable via one of these owners, e.g. `12345.chars`,
    // reachable only via a `Cool` row since `Int` has no `chars` row of its
    // own). Hand-probed against real samples: `Cool` against an `Int(2)` and
    // a numeric-parseable `Str("5")` (both exercise the same bridging
    // coercion `"abc".abs` fails to reach -- a plain non-numeric `Str` is
    // content-dependently unrecognized, matching `native_probe_recognizes_
    // per_type_methods`'s existing `Str` sample discrimination, so a
    // numeric-looking sample was used instead of widening that discrimination);
    // `Any`/`Mu` against `Value::package(Symbol::intern(owner))` (the bare
    // type object -- the same sample `eighth_slice_tail_rows_are_backed_by_
    // the_cascade` re-verifies every `Any`/`Mu` row against below, so a
    // definite value like `Int(2)` would over-claim: `Int(2).reverse` is
    // native-recognized but the abstract `Any.reverse` is not, confirmed
    // against real `raku` returning `((Any))` for the latter where mutsu
    // currently errors "No such method" -- a genuine dispatch gap, not this
    // slice's concern, but the row's arity bits must reflect it); `Code`
    // (the folded owner for
    // `Sub`/`Method`/`Block`/`Routine`, see `canonical_builtin_owner`)
    // against a real `sub ($a,$b) {...}` value; `Signature` against
    // `&sub.signature`; `IO::Path` against `"tmp".IO`; `IO::Handle` against
    // a FRESH `"...".IO.open(:w)` handle per candidate name (a handle is
    // stateful, so probing `close` early would starve every later candidate
    // of a real open handle). Candidate names are each owner's declared
    // `builtin_type_method_names` list (`COOL_OWN`+`NUMERIC_COERCIONS`,
    // `ANY_METHODS`, `MU_METHODS`, `CODE_METHODS`, `SIGNATURE_METHODS`,
    // `IO_PATH_METHODS`, `IO_HANDLE_METHODS`) -- every declared name gets a
    // row regardless of probe outcome (`.^can` in real Raku is a static
    // "does the class declare this" check, confirmed via `raku -e 'say
    // Cool.can("abs")'` returning true independent of receiver content), so
    // a name the probe cascade does not recognize at any pure arity still
    // gets a conservative `N`/`SPECIAL` row (existence, not invocation, is
    // what E7 step 4 needs) rather than being omitted. Six `Any` and two
    // `Signature`/two `Mu` candidates already had rows from earlier E2b
    // slices (`so`/`not`/`defined`/`WHERE`/`gist`/`raku` on `Any`,
    // `defined` on `Mu`, `gist`/`raku` on `Signature`) and were left as-is
    // rather than duplicated.
    ("Cool", "substr", 6, 9),
    ("Cool", "chars", 1, 9),
    ("Cool", "codes", 1, 9),
    ("Cool", "chomp", 1, 9),
    ("Cool", "chop", 3, 9),
    ("Cool", "contains", 2, 9),
    ("Cool", "comb", 7, 9),
    ("Cool", "ends-with", 2, 8),
    ("Cool", "fc", 1, 9),
    ("Cool", "flip", 1, 9),
    ("Cool", "index", 2, 9),
    ("Cool", "indices", 8, 12),
    ("Cool", "lc", 1, 9),
    ("Cool", "lines", 3, 9),
    ("Cool", "match", 8, 12),
    ("Cool", "ords", 1, 9),
    ("Cool", "pred", 1, 9),
    ("Cool", "rindex", 2, 9),
    ("Cool", "samecase", 2, 9),
    ("Cool", "split", 6, 9),
    ("Cool", "starts-with", 2, 8),
    ("Cool", "succ", 1, 9),
    ("Cool", "tc", 1, 9),
    ("Cool", "trim", 1, 9),
    ("Cool", "trim-leading", 1, 9),
    ("Cool", "trim-trailing", 1, 9),
    ("Cool", "uc", 1, 9),
    ("Cool", "words", 3, 9),
    ("Cool", "wordcase", 1, 9),
    ("Cool", "abs", 1, 9),
    ("Cool", "ceiling", 1, 9),
    ("Cool", "floor", 1, 9),
    ("Cool", "round", 3, 9),
    ("Cool", "sign", 1, 9),
    ("Cool", "sqrt", 1, 9),
    ("Cool", "log", 3, 9),
    ("Cool", "log10", 1, 9),
    ("Cool", "exp", 3, 9),
    ("Cool", "is-prime", 1, 9),
    ("Cool", "chr", 1, 9),
    ("Cool", "base", 6, 9),
    ("Cool", "polymod", 8, 13),
    ("Cool", "Numeric", 1, 8),
    ("Cool", "Int", 1, 8),
    ("Cool", "Num", 1, 8),
    ("Cool", "Rat", 3, 9),
    ("Cool", "Bool", 1, 9),
    ("Cool", "Str", 3, 9),
    ("Cool", "gist", 1, 9),
    ("Cool", "raku", 1, 9),
    // ADR-0019 E11 slice 2 (2026-08-14, follow-up): the native-int-coercion
    // method family (`42.int8`, `"42".byte`, ...), genuinely dispatched via
    // `target.isa_check("Cool")` for the eleven real coercion names,
    // unconditionally for the whole `Cool` family (`methods_0arg/mod.rs`).
    // Found via a `MUTSU_VM_STATS=1` sweep of the full `t/` suite after the
    // rest of this slice landed: one can-shadow mismatch remained
    // (`class=List method=int8 real=true shadow=false`, `t/native-int-
    // coerce-methods-are-cool-only.t`) alongside the known-and-out-of-scope
    // `class=Cancellation method=cancel` (see the ADR E11 progress note).
    // Hand-probed against the same `Int(2)`/`Str("5")` pair as the rest of
    // the `Cool` rows above, plus the bare `Cool` type object (all eleven
    // recognize at arity 0 there too, unlike most of the `Cool` block).
    //
    // **Correction (ADR-0019 Phase F box F3 step 2, 2026-08-15):** this
    // comment used to say these were "NOT in `COOL_OWN` ... deliberately
    // excluded from `.^methods`/`.^can`-by-list to avoid making every value
    // spuriously 'can' a C-width alias like `bool`". That conflated two
    // different lists: `is_native_int_coerce_method`'s exclusion concern is
    // about `NATIVE_INT_TYPES` (`bool`/`long`/`ulong`/... name a *type*, not
    // a method, and must never be dispatched as one) and is unrelated to
    // whether the eleven real coercion methods appear in `COOL_OWN`. `.^can`
    // already answered correctly for these eleven via this row's own arity
    // cascade regardless of `COOL_OWN` membership (`t/native-int-coerce-
    // methods-are-cool-only.t` pins exactly that). Only `.^methods`
    // enumeration was missing them, confirmed a genuine gap against real
    // Rakudo's `Cool.^methods` (F3 step 2's raku-verification triage) and
    // now closed via `COOL_NATIVE_INT_COERCE_TAIL` (`builtin_type_
    // methods.rs`), appended after `NUMERIC_COERCIONS` to match this block's
    // own position in `RAW_ROWS`.
    ("Cool", "int8", 1, 9),
    ("Cool", "int16", 1, 9),
    ("Cool", "int32", 1, 9),
    ("Cool", "int64", 1, 9),
    ("Cool", "uint8", 1, 9),
    ("Cool", "uint16", 1, 9),
    ("Cool", "uint32", 1, 9),
    ("Cool", "uint64", 1, 9),
    ("Cool", "byte", 1, 9),
    ("Cool", "int", 1, 9),
    ("Cool", "uint", 1, 9),
    ("Any", "say", 8, 12),
    ("Any", "put", 8, 12),
    ("Any", "print", 8, 12),
    ("Any", "note", 8, 12),
    // ADR-0019 E2b: universal Any/Mu methods, added by hand (not probed via
    // `builtin_sample_value`, which has no representative sample for an
    // abstract owner) after `dispatch_owner_coverage`'s 2026-08-10 sweep
    // showed these dominating `native_call_unmodeled` (Str x so alone was
    // 54% of ~38k hits) -- `dispatch_core_str::dispatch`/`dispatch_core_coerce::dispatch`
    // recognize `so`/`not`/`defined` unconditionally for every receiver type
    // (see the `try_dispatch!` chain in `methods_0arg/mod.rs`), so one row
    // per name at the owner that actually declares it (`Any`) is correct and
    // complete once the coverage check walks the MRO chain to find it (see
    // `Interpreter::record_native_row_coverage`) rather than doing a flat
    // point lookup at the receiver's own concrete owner.
    ("Any", "so", 1, 8),
    ("Any", "not", 1, 8),
    ("Any", "defined", 1, 9),
    ("Any", "WHAT", 8, 12),
    ("Any", "WHERE", 1, 8),
    ("Any", "HOW", 8, 12),
    ("Any", "WHY", 8, 12),
    ("Any", "iterator", 8, 12),
    ("Any", "flat", 7, 8),
    ("Any", "eager", 8, 12),
    ("Any", "lazy", 1, 8),
    ("Any", "map", 8, 12),
    ("Any", "grep", 8, 12),
    ("Any", "first", 8, 12),
    ("Any", "sort", 8, 12),
    ("Any", "reverse", 8, 12),
    ("Any", "unique", 1, 8),
    ("Any", "repeated", 1, 8),
    ("Any", "squish", 8, 12),
    ("Any", "head", 3, 8),
    ("Any", "tail", 3, 8),
    ("Any", "skip", 8, 12),
    ("Any", "min", 8, 12),
    ("Any", "max", 8, 12),
    ("Any", "minmax", 8, 12),
    ("Any", "elems", 1, 8),
    ("Any", "end", 1, 8),
    ("Any", "keys", 8, 12),
    ("Any", "values", 8, 12),
    ("Any", "kv", 8, 12),
    ("Any", "pairs", 8, 12),
    ("Any", "antipairs", 8, 12),
    ("Any", "classify", 8, 12),
    ("Any", "categorize", 8, 12),
    ("Any", "join", 1, 8),
    ("Any", "pick", 3, 8),
    ("Any", "roll", 1, 8),
    ("Any", "sum", 8, 12),
    ("Any", "reduce", 8, 12),
    ("Any", "produce", 8, 12),
    ("Any", "rotor", 8, 12),
    ("Any", "batch", 2, 8),
    ("Any", "Bool", 1, 8),
    ("Any", "Str", 2, 8),
    ("Any", "gist", 1, 8),
    ("Any", "raku", 1, 8),
    ("Any", "Numeric", 8, 12),
    ("Any", "Int", 8, 12),
    ("Mu", "WHAT", 8, 12),
    ("Mu", "WHERE", 1, 8),
    ("Mu", "HOW", 8, 12),
    ("Mu", "WHY", 8, 12),
    ("Mu", "WHICH", 1, 8),
    ("Mu", "Bool", 1, 8),
    ("Mu", "Str", 2, 8),
    ("Mu", "gist", 1, 8),
    ("Mu", "raku", 1, 8),
    ("Mu", "clone", 1, 8),
    ("Mu", "new", 8, 12),
    ("Code", "name", 8, 12),
    ("Code", "signature", 8, 12),
    ("Code", "arity", 8, 12),
    ("Code", "count", 8, 12),
    ("Code", "of", 8, 12),
    ("Code", "returns", 8, 12),
    ("Code", "Bool", 1, 9),
    ("Code", "Str", 3, 9),
    ("Code", "gist", 8, 13),
    ("Code", "raku", 8, 13),
    ("Signature", "params", 8, 12),
    ("Signature", "arity", 8, 12),
    ("Signature", "count", 8, 12),
    ("Signature", "returns", 8, 12),
    ("Signature", "Bool", 1, 9),
    ("Signature", "Str", 2, 9),
    ("Signature", "gist", 1, 8),
    ("Signature", "raku", 1, 8),
    ("IO::Path", "absolute", 8, 12),
    ("IO::Path", "basename", 8, 12),
    ("IO::Path", "cleanup", 8, 12),
    ("IO::Path", "copy", 8, 12),
    ("IO::Path", "dirname", 8, 12),
    ("IO::Path", "e", 8, 12),
    ("IO::Path", "d", 8, 12),
    ("IO::Path", "f", 8, 12),
    ("IO::Path", "l", 8, 12),
    ("IO::Path", "r", 8, 12),
    ("IO::Path", "w", 8, 12),
    ("IO::Path", "x", 8, 12),
    ("IO::Path", "rw", 8, 12),
    ("IO::Path", "rwx", 8, 12),
    ("IO::Path", "s", 8, 12),
    ("IO::Path", "z", 8, 12),
    ("IO::Path", "extension", 8, 12),
    ("IO::Path", "IO", 8, 12),
    ("IO::Path", "lines", 2, 9),
    ("IO::Path", "mkdir", 8, 12),
    ("IO::Path", "modified", 8, 12),
    ("IO::Path", "accessed", 8, 12),
    ("IO::Path", "changed", 8, 12),
    ("IO::Path", "mode", 8, 12),
    ("IO::Path", "move", 8, 12),
    ("IO::Path", "open", 8, 12),
    ("IO::Path", "parent", 8, 12),
    ("IO::Path", "parts", 8, 12),
    ("IO::Path", "pred", 8, 13),
    ("IO::Path", "rename", 8, 12),
    ("IO::Path", "resolve", 8, 12),
    ("IO::Path", "rmdir", 8, 12),
    ("IO::Path", "sibling", 8, 12),
    ("IO::Path", "slurp", 8, 12),
    ("IO::Path", "spurt", 8, 12),
    ("IO::Path", "succ", 8, 13),
    ("IO::Path", "symlink", 8, 12),
    ("IO::Path", "link", 8, 12),
    ("IO::Path", "add", 8, 12),
    ("IO::Path", "child", 8, 12),
    ("IO::Path", "unlink", 8, 12),
    ("IO::Path", "volume", 8, 12),
    ("IO::Path", "watch", 8, 12),
    ("IO::Path", "words", 3, 9),
    ("IO::Path", "CWD", 8, 12),
    ("IO::Path", "SPEC", 8, 12),
    ("IO::Path", "Bool", 1, 9),
    ("IO::Path", "Str", 2, 9),
    ("IO::Path", "gist", 8, 13),
    ("IO::Path", "raku", 8, 13),
    ("IO::Path", "Numeric", 8, 12),
    ("IO::Path", "Int", 8, 12),
    ("IO::Handle", "open", 8, 12),
    ("IO::Handle", "close", 8, 12),
    ("IO::Handle", "path", 8, 12),
    ("IO::Handle", "IO", 8, 12),
    ("IO::Handle", "slurp", 8, 12),
    ("IO::Handle", "slurp-rest", 8, 12),
    ("IO::Handle", "spurt", 8, 12),
    ("IO::Handle", "lines", 2, 9),
    ("IO::Handle", "words", 3, 9),
    ("IO::Handle", "comb", 1, 9),
    ("IO::Handle", "split", 8, 13),
    ("IO::Handle", "print", 8, 12),
    ("IO::Handle", "print-nl", 8, 12),
    ("IO::Handle", "printf", 8, 12),
    ("IO::Handle", "say", 8, 12),
    ("IO::Handle", "put", 8, 12),
    ("IO::Handle", "get", 8, 12),
    ("IO::Handle", "getc", 8, 12),
    ("IO::Handle", "read", 8, 12),
    ("IO::Handle", "readchars", 8, 12),
    ("IO::Handle", "write", 8, 12),
    ("IO::Handle", "seek", 8, 12),
    ("IO::Handle", "tell", 8, 12),
    ("IO::Handle", "eof", 8, 12),
    ("IO::Handle", "flush", 8, 12),
    ("IO::Handle", "lock", 8, 12),
    ("IO::Handle", "unlock", 8, 12),
    ("IO::Handle", "opened", 8, 12),
    ("IO::Handle", "nl-in", 8, 12),
    ("IO::Handle", "nl-out", 1, 9),
    ("IO::Handle", "chomp", 8, 13),
    ("IO::Handle", "encoding", 8, 12),
    ("IO::Handle", "decode", 8, 12),
    ("IO::Handle", "Supply", 1, 9),
    ("IO::Handle", "native-descriptor", 8, 12),
    ("IO::Handle", "WRITE", 8, 12),
    ("IO::Handle", "READ", 8, 12),
    ("IO::Handle", "t", 8, 12),
    ("IO::Handle", "Bool", 1, 9),
    ("IO::Handle", "Str", 2, 9),
    ("IO::Handle", "gist", 8, 13),
    ("IO::Handle", "raku", 8, 13),
];
