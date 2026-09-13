# The hand-built `__mutsu_*` metadata key is gone, and the gate is now a ban

Stage 3 of [#8087](https://github.com/tokuhirom/mutsu/issues/8087) took the
count of hand-built `format!("__mutsu_<ns>::{name}")` key sites in `src/` from
174 to **zero**. `scripts/check-magic-keys.sh` was a ratchet over a per-file
baseline while that debt was being paid down; with nothing left to ratchet it
became the simpler thing it was always heading for — a ban, with no baseline
file at all. `MetaNs` (`src/runtime/meta_ns.rs`) is now the only place in the
interpreter where one of these key prefixes is spelled.

## What moved

45 namespaces, up from 11. Two families, and they are shaped differently:

* **Env keys**, `__mutsu_<ns>::<name>`, keyed by `Symbol`. The element store's
  own metadata is here (`deleted_index`, `ro_index`, `elem_share`,
  `bound_decont`, `array_share`), along with `constant_var`, `outer`,
  `gather_self_ref`, `deep_readonly`, `atomic_name`, `shared_dirty`,
  `var_source_name` and a dozen more.
* **Mixin-registry keys**, `__mutsu_role__<role>` / `__mutsu_attr__<attr>`,
  which live in the `String`-keyed `MixinOverrides` map rather than in an `Env`
  and join prefix to name with `__` rather than `::`. These are 78 of the 174
  sites and include the single most-probed key in the interpreter:
  `__mutsu_role__` is read by every `does`, every method dispatch onto a mixin
  and every type smartmatch.

`MetaNs::prefix` had asserted a trailing `::`; it now carries each namespace's
own separator, and `MetaNs::pair_sep` carries the one that joins a two-part
key's halves — `::` everywhere except `__mutsu_attr_trait__<owner>!<attr>`,
which has used `!` since long before this type existed.

Conversion was not merely onto the constructor. On the env side the hot sites
hold the key as a pre-interned `Symbol` and probe with `get_sym` /
`get_mut_sym` / `remove_sym`, so neither the string nor its hash is rebuilt per
access; `Env::get` is `get_sym(Symbol::intern(key))`, so handing it a
`&'static str` would still have hashed the key on every lookup. On the mixin
side the map is `String`-keyed, so reads take `MetaNs::str_key_for_str` (a
`&'static str` out of the memo, no allocation) and only the inserts — which
need an owned `String` regardless — allocate.

Every converted write goes through `Env::insert_sym_noting`, never
`insert_sym`. That distinction is the one that bites: `insert_sym` deliberately
skips `note_env_key`, so a name-derived write landing on it stores the metadata
correctly *and* leaves the reader's `elem_index_meta_possible()` probe switched
off for the rest of the process. No error, no wrong type — the metadata is
simply never looked for again. Stage 2 shipped that bug and paid a CI cycle to
find it.

## Three namespaces are deliberately not memoized

`AtomicValue`, `PredictiveSeqIter` and `FfState` are keyed by an *identity* —
a `Seq`'s id, a flip-flop's dynamic scope — rather than by a name, and an
identity is fresh every time. Memoizing those would be a map that only ever
grows, which is a leak wearing a cache's clothes. They go through
`MetaNs::key_for_id` / `MetaNs::owned_key_from_parts`, which build the string
every time on purpose. They are routed through `MetaNs` for the *other* half of
its job — one place that spells the prefix — not for the memo.

## The gate had a hole, and it was hiding real sites

The ratchet matched `format!("__mutsu_`, which means it could not see a
`format!` whose literal sits on the **next line** — which is how rustfmt writes
any call that does not fit on one. Eight real sites were behind that, including
one in `__mutsu_callable_id::`, the very namespace stage 2 had just declared
clear, and three more in a three-part `__mutsu_inline_package_sub_preregistered`
key that no previous survey had counted.

The gate now matches the format-string *literal* instead —
`"__mutsu_<ns>::{"` or `"__mutsu_<ns>__{"` — which sees both layouts. The `::`
or `__` in that pattern is also what makes the gate precise, because it encodes
a convention the codebase already keeps without exception:

| shape | what it is |
| --- | --- |
| `__mutsu_<ns>::<name>` | an env key, derived from a binding's name |
| `__mutsu_<ns>__<name>` | a mixin-registry key, derived from a role/attr name |
| `__mutsu_<kind>_<n>` | a gensym — a counter after the `_`, not a name |

Only the first two are what #8087 is about. They are *probed*: rebuilt from a
name something else also has, over and over at runtime, which is why memoizing
them pays and why a spelling mismatch between writer and reader silently loses
metadata. The ~40 gensyms are built once at compile time and handed straight to
`alloc_local`; nothing ever looks one up by rebuilding it, so there is nothing
to memoize and nothing to get out of step. Folding them into `MetaNs` would
have been actively wrong — a memo keyed by a monotonically increasing counter
is a leak — so the gate names the distinction instead of erasing it.

## Tests

`meta_ns.rs` pins the exact spelling every namespace produces, and now fails if
a variant is added to `MetaNs::ALL` without one; it also checks that no
prefix is a prefix of another, which `__mutsu_role__` / `__mutsu_role_seq__`
and `__mutsu_attr__` / `__mutsu_attr_trait__` each come one character from
violating — and `MixinOverrides::seed_missing_attributes` really does enumerate
that map with `strip_prefix`.

Spelling tests cannot catch a *behavioural* break, so two prove files carry
that: `t/oo/role-mixin-marker-keys.t` (14 cases — role markers, application
order, parameterisation, composed attributes, candidate groups) and
`t/vm/binding/element-and-scope-metadata-keys.t` (16 cases — deleted and
readonly indices, element and array sharing, `constant`, `$OUTER::`, `gather`,
deep readonly). Both pass under rakudo as well as mutsu, so they pin Raku
behaviour rather than mutsu's current answer.

## What is still open

Stage 4, the actual fix: these keys should not exist at all. Each is a property
of one binding, stored as a *sibling entry in the same env as the binding
itself* purely because there is nowhere else to put it. Moving them onto the
resolved container descriptor (#8069 §4.1) or off the per-frame env entirely
(#7817 / ADR-0084) turns the probe into a field read — no key, no memo, no hash,
nothing to ban. `MetaNs` makes that a change at one site per namespace instead
of at twenty-eight, which is the only reason it exists.
