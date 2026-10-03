# `.raku`/`.gist` rendering moves into `value`; signatures stop naming the interpreter

Slice 7 of the layer decomposition (#10779) cut the upward references from the
lower layers from 76 to 71.

- `.raku` rendering (`value/raku_repr.rs`, with the `Match` helpers in
  `value/match_helpers.rs`), `.gist` rendering (`value/gist.rs`) and the
  structured exception messages (`value/exception_message.rs`) now live in
  `value`, which already called them when building error values. The old paths
  (`builtins::methods_0arg::raku_repr`, `runtime::utils::gist_value`, ...) stay
  as re-exports.
- Building a `Parameter` or `Signature` value needed `&Interpreter` only to
  resolve a `subset`'s base type. It now takes the one-method trait
  `value::signature::SubsetBases`, which the interpreter implements, so
  `value/signature.rs` and `value/error_typed.rs` no longer name the runtime.
- The line splitter behind a Supply's `.lines` moved to `value/split_lines.rs`
  (its `limit` parameter had no remaining caller and is gone).
