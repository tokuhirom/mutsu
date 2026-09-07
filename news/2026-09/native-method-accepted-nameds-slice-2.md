# Every builtin-layer entry now honours the accepted-named declaration

[ADR-0070](../../docs/adr/0070-native-methods-declare-the-named-arguments-they-accept.md)
gave builtin methods a declaration of the named arguments they accept, and had
the *arity cascade* drop the rest. Slice 2 finishes the job: it declares the
residue of the same Rakudo survey, implements the one adverb the table listed
but mutsu did not honour (`base(:no-trailing-zeroes)`), and — the part that
turned out to be the real work — routes the **six other places the builtin
layer is entered** through the same declaration.

## The metric

`scripts/native-method-adverb-sweep.raku` is new, and is the measurement the
ADR described but never shipped as a script. It derives 1 422 (receiver, method,
argument-shape) probes from `src/builtins/native_method_row_table.rs` and, for
each, compares `R.M(A)` against `R.M(A, :qqzz9)`. `:qqzz9` is a name no Raku
routine declares, so under a conforming implementation the two must agree. It
runs under any interpreter, so `raku` supplies its own baseline:

| | not named-blind, of 1 422 probes |
|---|---|
| mutsu, before | **78** |
| mutsu, after | **18** |
| `raku` (the baseline: calls Rakudo itself rejects) | 23 |

Of the 18, only 9 are mutsu-specific, down from 72.

## What was actually wrong

### `elems` on an instance recursed until the stack overflowed

The sweep could not even run: `Blob.new(1,2,3).elems(:qqzz9)` **crashed the
process**. `builtin_elems` is defined as `$x.elems`, and
`dispatch_elems_method` delegates back to `call_function("elems", ...)` — a
cycle that terminates only when the inner call is served by the arity cascade.
`native_fastpath_receiver_state_guard` deliberately diverts *every* `Instance`
receiver's `.elems` away from that cascade (so a `Supply` reaches its own arm),
so for any other instance the two bounced forever.

That is not a named-argument bug at all: `elems(Date.new(2026,1,1))`,
`elems(Blob.new(1,2,3))` and `elems(C.new)` for a plain user class all
stack-overflowed on `main`. An instance now answers from the native 0-arg layer
directly, which is where the bounce was trying to arrive.

### The declaration reached one entry, and the builtin layer has seven

The ADR wired the declaration into the arity cascade, its interpreter twin and
`dispatch_method_by_name_1/2/3`. Everything else that dispatches a builtin
*before* the cascade kept counting an adverb as a positional. Each of these is
now routed through `strip_undeclared_nameds`, restricted so a user class's own
named parameter is never at risk:

- `vm_baghash_mutators::apply_baghash_mutator` — `BagHash.new(1,2,2).add(1, :zzz)`
  died "Too many positionals passed; expected 2 arguments but got 3"; raku adds
  the element and answers `Nil`.
- the `tail` interceptor in `call_method_with_values_inner` — `(1,2,3).tail(:zzz)`
  died "Cannot use 'zzz\tTrue' as a tail count".
- the two by-value array/hash mutator blocks — `[1,2,3].pop(:zzz)` died,
  `%h.push(:zzz)` inserted a `zzz` key.
- `exec_call_method_mut_op_impl` and `call_method_mut_with_values`, the mutable
  opcode and its dispatch entry — `@a.push(:zzz)` left `[1, 2, 3, :zzz]`.
- `call_native_instance_method` and `try_io_path_lexical`, the native-instance
  entries — `"/tmp".IO.sibling(:zzz)` built `IO::Path.new("/zzz\tTrue")`.
- the "compose a method over a callable" last resort — `{ $_ }.arity(:zzz)`
  answered a `<composed-method:arity>` **Sub**, because the real `.arity` arm is
  guarded on `args.is_empty()` and had already declined.

The compiler's `@a.push(x)` → `ArrayPush` fast path also bails out on a named
call site now: that opcode stores whatever single value it is handed and has no
notion of named-ness.

A *positional* `Pair` is still data everywhere (`@a.push((k => 9))`,
`rotor(2 => -1)`, a `Pair` held in a variable) — named-ness is a call-site
property, ADR-0021.

### `base(:no-trailing-zeroes)` was declared but not implemented

The ADR recorded that "the adverb reaches the implementation". Re-measured: it
does not. `.base($radix, $digits, :no-trailing-zeroes)` is a three-argument
call that matches no arity arm, so the implicit-`*%_` retry dropped the adverb
before the 2-ary `base` ever saw it. `native_base_with_options` is a new
interceptor in front of the cascade, consulted from both the VM and the
interpreter entry.

The semantics were established against `raku` over 21 cases, and the adverb is
narrower than it looks: it is declared on **`Rational.base` only**.
`255.base(16, 4, :no-trailing-zeroes)` is still `"FF.0000"` and
`2.5e0.base(10, 5, :no-trailing-zeroes)` still `"2.50000"`; `0.5` gives `"0.5"`,
and `1.0` gives `"1"` — when every fractional digit goes, so does the radix
point.

## The survey's blind spot, measured

The `todo/deep/` file asked for the `%_`-slurpy family (`grep`, `first`,
`subst`, `trans`, `reduce`, `produce`, `keys`, `values`, `kv`, `pairs`, `list`,
`Array`) to have its accepted set "confirmed by hand". That turned out to matter
for exactly the reason it warned about. A refinement of the survey — reporting
the *name* of each named slurpy rather than just its presence — separates the
implicit `*%_` every method carries from a slurpy the routine declares and
reads (`subst`'s `*%options`, `first`'s `*%a`). But it is still only a lower
bound: `Str.trans` declares nothing but `*%_` and nevertheless reads `:d`,
`:s` and `:c` out of it. So `trans` and `subst` stay undeclared, while the rest
of the family — behaviourally verified against Rakudo, which ignores `:qqzz9` on
every one of them — is declared.

35 method names were added to the table, plus six with a non-empty accepted set
(`Str`'s `:subscript`/`:superscript`, `raku`'s `:arglist`, `map`'s six plus
hyper's `:batch`/`:degree`, `classify-list`/`categorize-list`'s `:as`,
`unique`'s `:as`/`:with`/`:expires`, `Numeric`'s `:fail-or-nil`).

## Pins

`t/native-method-accepted-nameds.t` grows from 66 to 123 assertions, and — as
before — the **whole file passes unmodified under real `raku`**, so it is a
conformance test rather than a snapshot of mutsu's behaviour.
