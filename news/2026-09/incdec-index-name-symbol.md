# The element inc/dec opcode stops re-interning its variable name

`exec_inc_dec_index_op` — the opcode behind `%h{$k}++` and `@a[$i]++` — read its
variable's name out of the chunk's constant pool as a `&str` and then let every
probe on that name rediscover the corresponding `Symbol` for itself.
`Symbol::intern` is a thread-local, string-keyed hash lookup (a `LocalKey`
access, a `RefCell` borrow, an `FxHashMap<String, Symbol>` probe and the
`memcmp` that confirms the hit), so the same name was re-hashed three times per
increment:

| call site | calls | Ir |
| --- | ---: | ---: |
| `get_env_with_main_alias` | 100,000 | 15.39 M |
| `var_hash_key_constraint` | 100,001 | 15.39 M |
| `Env::get_mut` | 100,000 | 15.39 M |

The fix hoists `code.const_sym(name_idx)` once — `const_sym` interns once per
constant slot for the life of the chunk — and threads that `Symbol` through
every probe the opcode makes on the name: the declared type constraint, the
element-container read, the object-hash key-type lane, and the write-back.
`var_type_constraint_sym`, `get_env_with_main_alias_sym` and `Env::get_mut_sym`
already existed; `var_hash_key_constraint_sym` is new, added alongside them. Its
attribute fallback still takes the `&str`, because an object-hash *attribute*
(`has Callable %!Conv{Mu:U}`) is not a lexical — its key type comes from the
class registry, resolved by spelling.

## Measured

`benchmarks/word-count.raku`, whose inner loop is `%counts{$w}++`, under
callgrind. Instruction counts are deterministic and layout-insensitive, which
matters here: this box has no `perf`, and issue #7579's own method notes record
that the ~5% code-layout lottery makes a cross-build cycle comparison unable to
support a conclusion at this effect size.

| | before | after | delta |
| --- | ---: | ---: | ---: |
| `word-count` total | 1,136,132,456 | 1,046,851,308 | **−7.86%** |
| `bench-string` (control) | 530,662,736 | 530,857,721 | +0.04% |

The removal is visible directly in the profile rather than only in the total:
`Symbol::intern`'s self time (20.69 M) and the intern cache's `LocalKey::with`
(33.90 M) both leave the profile entirely, and `__memcmp_avx2_movbe` falls from
33.47 M to 21.67 M as the cache's string comparisons stop happening.

Two controls establish that this is work *removed* rather than work *skipped* —
the failure mode where a change measures faster because it quietly stopped doing
something. `malloc`/`_int_free` are unmoved (46.64 M / 72.02 M before, 46.63 M /
72.06 M after), so nothing was allocated or freed differently; and
`benchmarks/word-count.raku`'s output is byte-identical across the two builds.
`bench-string` is the negative control: it contains no `%h{$k}++`, and it does
not move.

## A correction to issue #7579's item 4

The ticket's item 4 predicted that the remaining `Symbol::intern` cost lived in
roughly 75 call sites that probe the env with a *literal* key — `env().get("_")`,
`env().get("/")`, `env().get("!")` — and proposed sweeping them onto the
well-known pre-interned symbols. The profile does not support that. `Env::get`'s
string-keyed form is called **60 times in the whole of `word-count`**; a sweep of
those sites would have bought essentially nothing on this benchmark.

The cost is in names that are only known at *runtime* — a variable name coming
out of a constant pool, re-interned per execution — not in fixed literals. That
distinction is what this change acts on, and it is worth carrying into any
further work on the ticket: prefer looking for a `&str` that a caller already
holds a `Symbol` for over looking for a string literal.

## Pinned

`t/incdec-index-name-symbol.t` exercises each probe the change touches: the
object-hash key-type lane and the declared key type surviving an increment
(`my %h{Int}`, `my %h{Any}`), the attribute fallback that still resolves by
string, the typed-container autovivification lane (`BagHash`, `MixHash`), and
increments through shared and captured containers. Verified against Rakudo as
the oracle.
