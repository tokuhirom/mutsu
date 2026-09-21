# `++`/`--` on a typed lexical stops re-deriving its constraint on every iteration

[#8877](https://github.com/tokuhirom/mutsu/issues/8877) named the `SetLocal`
store path as the first consumer of a typed lexical's declared constraint
probed through the env chain on every access, and
[the first slice](a-typed-lexicals-constraint-stops-being-an-env-probe-per-store.md)
baked it onto `BindingDesc::declared_constraint` for that one caller. The issue
named it "the one caller that asks", and left the rest of `MetaNs::Type`'s
~120 other readers as future work.

`postfix:<++>`/`postfix:<-->`/`prefix:<++>`/`prefix:<-->` are a second, equally
hot caller of the same fact. `wrap_native_int_arithmetic_result_for` —
"was this variable declared `int8`, and does the arithmetic result need
wrapping for it?" — ran unconditionally on every `++`/`--`, whether or not the
variable was ever declared with a native width, re-deriving the answer from
`__mutsu_type::<name>` every time.

## The slot knows this too

`wrap_native_int_arithmetic_result_for_slot` reads
`BindingDesc::declared_constraint` first, when the opcode carries a
compile-time-resolved local slot (every `++`/`--` on a plain lexical does),
behind the same `subsets.is_empty()` guard `native_typed_store_is_identity`
uses (a `subset int of ...` can redirect a native type name):

* `NativeInt` — `int`/`int64`/`atomicint` all wrap at the same 64-bit signed
  boundary (`native_types::native_type_bits`), so the exact declared spelling
  doesn't matter to the wrap itself. Calling
  `wrap_native_int_arithmetic_for_constraint("int64", value)` directly is
  correct for both an in-range `Int` (the identity) and the rarer case where
  the increment already overflowed into a `BigInt` — that arm wraps at the
  same boundary regardless of which spelling produced it.
* `NativeStr` / `NativeNum` — the wrap is always the identity for a
  non-native-int constraint, for any value shape, so these return the value
  straight back.
* `NonNative` / `Unrecorded` / `Conflicting` — fall through to the env probe
  exactly as before; a narrow width (`int8`, `uint32`, ...) needs the real
  string to pick its bound, which the coarse bake does not carry.

Wired into the four opcodes' hot tail paths — the plain-lexical case, not the
attribute-cell/`ContainerRef`/`Proxy` branches those functions also handle.

## Measured

`my int $i = 0; while $i < 100000 { $i++ }`, paired callgrind runs (`git
checkout HEAD~1 -- <the four changed files>` for "before", `--cache-sim=no
--branch-sim=no`, second run of each side per the warm-run rule):

| | before | after | |
| --- | ---: | ---: | ---: |
| whole program | 503,087,297 | 469,988,611 | **-6.58%** |
| `wrap_native_int_arithmetic_result_for[_slot]`, inclusive, per call | 745 | 409 | **-45.1%** |

That is **331 instructions per iteration** of a loop whose entire body is one
`++`. The caller tree shows where it went:

| | before | after | |
| --- | ---: | ---: | ---: |
| `var_type_constraint_*` (env-scoped constraint lookup) | 20,700,069 | 13,800,069 | -33.3% |
| `Env::get_sym` | 18,001,462 | 14,401,462 | -20.0% |
| `nanbox::payload_op` | 13,207,494 | 8,807,494 | -33.3% |
| `nanbox::peek::view_kind` | 9,004,703 | 6,404,703 | -28.9% |
| `malloc` / `free` / `_int_free` | 17.7M / 15.9M / 22.2M | 13.5M / 12.0M / 16.8M | -23.8% / -24.5% / -24.4% |

The residual `var_type_constraint_*`/`Env::get_sym` calls are a *different*
probe this slice did not touch — `store_named_scalar_rmw_result`'s own
readonly/shared-cell checks consult the env by name regardless, so the count
drops by a third rather than to zero. The malloc/free reduction is the same
`Arc`-bump-then-drop the store slice's write-up already named: the env's
`Str` value for the constraint was cloned out of the map on every probe this
slice removes.

Pinned by `t/vm/binding/declared-constraint-incdec-bake.t`, mirroring
`declared-constraint-slot-bake.t`'s shapes for the store path: narrow widths
still wrap on `++`/`--` (including through a `BigInt` overflow at the `int64`
boundary), one slot with two constraints still poisons and defers, a subset
redirecting a native name still works, and a hot `++` loop still accumulates
correctly. All 15 assertions match rakudo.

## Not the whole issue

#8877 stays open. `MetaNs::Type`'s namespace, its ~120 remaining readers,
`Symbol::type_meta_subject`, and the closure-capture filter's
`__mutsu_type::` handling are all still there — this takes the hot reader off
it for a second call site, following the same pattern the store slice used,
not the namespace retirement the issue actually asks for.

## A pre-existing bug found along the way

Writing the attribute-shaped case for the new test surfaced
[#8985](https://github.com/tokuhirom/mutsu/issues/8985): a narrow native-int
*attribute* (`has int8 $.v`) does not wrap on overflow when incremented
through `$!v++` in a method — `127` becomes `128`, not `-128`. The lexical and
array-element equivalents both already wrap correctly, so this is a gap
specific to the attribute-cell increment path, confirmed present on `main`
before this slice (reproduces identically with the change stashed out) and
unrelated to it — filed rather than fixed here, since the code this slice
touches is the plain-lexical tail path, not the attribute branch.
