# ADR-0016 P5: full inventory of `Match`-representation consumer sites

ADR-0016's P5 phase estimated "~34 `class_name == "Match"` consumer sites" to
funnel through accessor helpers before the representation can swap to a lazy
`Match`. A full sweep (2026-07-31, post-P2) found the real surface is
**72 attribute-touching sites**, plus 10 type-check-only sites and ~14 inert
registry strings. This file is the working inventory for the seam refactor;
delete it (git mv to `news/`) when P5's seam lands.

## Representation today

`ValueView::Instance { class_name: "Match", attributes: Gc<InstanceAttrs> }`
with a `Symbol`-keyed `AttrMap`. Attributes observed in the wild:

`str, from, to, orig, list, named, ast, silent_caps, sym_variant,
action_name, reduce_time_vars, capture_alias_map, actions, pos,
__failed_match__`

Two attrs beyond the ADR's list that the new repr must cover: `actions`
(grammar action object) and `__failed_match__` (+ `pos`), set on failed
`.subparse` matches (`methods_grammar.rs` `make_failed_match_value`).

There are no accessor helpers today; the closest are three private duplicated
readers: `match_from` (`runtime/utils/gist.rs`) and `match_value_from`/
`match_value_to` (`builtins/methods_0arg/match_helpers.rs`).

## Counts

| Group | Sites |
|---|---|
| (a) Builders — `make_instance("Match")` | 9 |
| (a') Rebuilders — clone attrs → insert → re-`make_instance` | 7 |
| (b) Scalar readers (`str`/`from`/`to`/`orig`/`action_name`/`__failed_match__`) | 18 |
| (c) Structure readers (`list`/`named`) | 37 |
| (d) In-place mutators (`attributes.insert` on live instance) | 1 |
| **Total attribute-touching** | **72** |

## Key structural facts for the seam

- **Builders funnel already.** Essentially all regex-produced Matches go
  through three constructors: `Value::make_match_object_full(_q)`
  (`value_methods_c.rs`) and `make_match_object_with_captures`
  (`value_methods_b.rs`) — 24 call sites across 16 files. Plus `Match.new`
  (`methods_object_native_ctors_misc.rs`) and `make_failed_match_value`
  (`methods_grammar.rs`). Builder-side swap is cheap.
- **The proposed 4 accessors cover 55/72** (groups b+c): `match_str`/
  `match_span` absorb (b), `match_list`/`match_named` absorb most of (c).
  Three more are needed to close the set: `match_ast` (ast/made — 3 read +
  5 write sites), `match_meta` (sym_variant/action_name/silent_caps/
  reduce_time_vars/capture_alias_map/actions — 9 sites, all but one in
  `methods_grammar.rs`), `match_is_failed` (`__failed_match__` — 3 sites:
  `methods_0arg/mod.rs`, `gist.rs`, `types_truthy.rs`).
- **Hardest sites: the 7 rebuilders + 1 in-place mutator.** Rebuilders do
  `InstanceAttrs::clone` → `insert` → `make_instance` (the `.made`/`ast`
  write path): `methods_grammar.rs` ×2, `regex_eval_repeat.rs` ×3,
  `methods_call_dispatch.rs` `Match.make`, `vm_call_method_mut_ops.rs`
  `Match.make` (the last two are near-duplicates — collapse into one helper).
  A lazy repr needs an explicit `with_ast(...)`-style copy-on-write helper
  first. The one in-place mutator is `seq_helpers/smart_match.rs`
  (`attributes.insert("orig"/"ast")` on the live `Gc`) — convert to the
  rebuild pattern or give the repr an override slot.
- **Biggest structural consumer**: the grammar action walk
  (`methods_grammar.rs` invoke_grammar_actions recursion) reads AND writes
  `named`/`list` on every node — it needs a mutable-tree seam, not just read
  accessors (ADR P5 already plans to rewrite it over `CapNode`).
- **Cheap first folds (zero risk)**: `gist.rs` `match_from` vs
  `match_helpers.rs` `match_value_from`/`to`; `map_hash_coerce.rs` vs
  `coerce_containers.rs` `%($/)` named-readers; `dispatch_core_repr.rs`'s two
  `match_gist` forwarders.

## Site tables

### (a) Builders — 9

| site | attrs written |
|---|---|
| `value_methods_c.rs` `make_capture_match` | str, from, to, list, named, orig |
| `value_methods_c.rs` quantified leaf (nested) | str, from, to, list, named, orig |
| `value_methods_c.rs` `make_subcap_match` | full 12-attr set |
| `value_methods_c.rs` quantified leaf (root) | str, from, to, list, named, orig |
| `value_methods_c.rs` root of `make_match_object_full_q` | str, from, to, orig, list, named, silent_caps |
| `seq_helpers/regex_captures.rs` leaf closure | str, from, to, list, named |
| `seq_helpers/regex_captures.rs` `apply_single_regex_captures` root | str, from, to, list, named |
| `methods_object_native_ctors_misc.rs` `build_native_match_value` | str, from, to, orig, list, named |
| `methods_grammar.rs` `make_failed_match_value` | str, from, to, pos, orig, list, named, __failed_match__ |

### (b) Scalar readers — 18

`builtins/methods_0arg/mod.rs` (.from/.to/.pos/.Str/.Bool/.orig/.target;
.prematch/.postmatch; .actions), `match_helpers.rs` (from/to),
`value/display.rs` (str), `value/value_eq.rs` (from/str),
`value/types_truthy.rs` (__failed_match__), `utils/compare.rs` (str),
`utils/gist.rs` `match_from`, `builtins_operators_coerce.rs` (str),
`dispatch_core_math.rs` (str), `vm/vm_misc_coerce.rs` (str),
`regex_token_method.rs` ×2 (orig/to), `regex_match_atom.rs` (to),
`seq_helpers/regex_captures.rs` (to), `methods_grammar.rs` `get_action_name`
(action_name), `methods_grammar.rs` (silent_caps/from).

### (c) Structure readers — 37

The `.list/.Array/.hash/.Hash/.keys/.values/.pairs/.kv/.elems/.ast/.made`
cluster in `methods_0arg/mod.rs`; `match_raku_repr`/`match_caps`/
`match_chunks` in `match_helpers.rs`; `match_gist` (+ `push_capture`) in
`utils/gist.rs`; `%($/)` coercions (`coerce_containers.rs`,
`map_hash_coerce.rs`); AT-POS/AT-KEY/EXISTS-KEY/EXISTS-POS
(`methods_narg/dispatch_1arg.rs`, `methods_dispatch_match3.rs`);
`methods_match_dispatch.rs`; `methods_string_subst_repl.rs`;
`seq_helpers/regex_captures.rs`, `seq_helpers/smart_match.rs`; the grammar
action walk cluster in `methods_grammar.rs` (leaf fast path, recursion,
sym_variant/reduce_time_vars readers); `regex_eval.rs`,
`regex_eval_repeat.rs`; `$/` subscript ops in `vm/vm_var_index_ops.rs` ×5 and
`vm/vm_smartmatch_ops.rs`.

### (a') Rebuilders — 7

`methods_grammar.rs` ×2 (ast/actions/capture_alias_map),
`regex_eval_repeat.rs` ×3 (ast; named/list; named),
`methods_call_dispatch.rs` `Match.make` (ast),
`vm_call_method_mut_ops.rs` `Match.make` (ast).

### (d) In-place mutator — 1

`seq_helpers/smart_match.rs` — `attributes.insert("orig")` / `insert("ast")`
on the live instance.

### (e) Type-check only — 10 (no repr coupling; leave as-is)

`str_match.rs`, `coercion.rs` (.Capture), `raku_repr.rs`,
`methods_signature.rs`, `methods_object_default_ctor.rs`,
`smart_match.rs`, `vm_smart_match.rs`, `vm_misc_typed_range.rs`,
`types_isa.rs` ×2.
