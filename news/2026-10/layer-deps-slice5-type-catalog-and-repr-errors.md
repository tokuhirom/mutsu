# Layer decomposition, slice 5: the type catalog moves down, repr-dependent builders move up

Issue #10779's ratchet on upward references from the lower layers (`make check-layer-deps`) went
from 111 to 86. Two directions this time — a helper that is pure moves *down* below `Value`, and a
helper that only the upper layers call but that needs their renderers moves *up* out of `Value`:

- **Down.** The built-in types' static MRO/roles catalog and the ancestry queries derived from it
  (ADR-0051) were pure tables in `builtins/`; they are now the leaf module `src/builtin_types/`
  (`catalog.rs`, `ancestry.rs`), so `Value`'s type checks no longer name `builtins`. The shaped
  array helpers (`is_shaped_array`, `shaped_array_shape`, leaves) moved to
  `src/value/shaped_array.rs`; the Mix weight decoder and its print rule to
  `src/value/mix_weight.rs`; `unicode_titlecase_first` to `src/ucd/case.rs`; and the
  native-backing attribute key to `value::types`.
- **Up.** Forcing a scan-reduction lazy list (`[\+] 1..*`) and the lazy `.pairs`/`.kv` index-pipe
  stage were `LazyList` methods that called the builtin arithmetic and range primitives, while only
  the runtime and VM ever called them; they are now free functions in `builtins/lazy_scan.rs`.
  Likewise the `RuntimeError` builders whose message renders the offending value with `.gist`/`.raku`
  (`X::Method::NotFound` with its "Did you mean" suggestion, `X::Parameter::RW` and three
  `X::TypeCheck::Binding::Parameter` forms) moved to `runtime::did_you_mean` and
  `runtime/utils/binding_errors.rs`.

Nothing changes in behavior. What remains in `Value` is mostly `value_to_list` (which needs the
string successor and the `Date` helpers), the `worker_pool` hooks behind promises, and the
parse-time calls in the parser that want a compile-time host trait.
