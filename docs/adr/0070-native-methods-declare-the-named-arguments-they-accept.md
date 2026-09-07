# ADR-0070: A builtin method declares the named arguments it accepts, and the arity cascade drops the rest

- Status: **Proposed** (slice 1 implemented 2026-09-07)
- Date: 2026-09-07
- Related: [ADR-0021](0021-argument-namedness-is-a-call-site-property.md)
  (argument named-ness is a call-site property),
  [ADR-0019](0019-compiled-declarations-and-unified-method-dispatch.md)
  (unified method dispatch entries, native method rows),
  `news/2026-08/native-methods-honour-the-implicit-slurpy-named.md` (the loud
  half of this bug), `todo/deep/native-method-accepted-named-declarations.md`

## Context

Every Raku *method* carries an implicit `*%_`, so a named argument the method
does not declare is silently swallowed and cannot change the answer. mutsu's
builtin methods are not dispatched by signature — they are dispatched by
**arity**, by `native_method_0arg` / `_1arg` / `_2arg` and by three by-name
handlers (`dispatch_method_by_name_1/2/3`), none of which has any notion of a
named argument. A call-site `Pair` therefore occupies a positional slot.

That has two symptoms, and only the first is fixed.

**1. The lookup misses (loud).** `4.log(:base(2))` counted the `Pair` as a
positional, looked up a 1-ary `log`, found none and died with
`X::Method::NotFound`. Fixed 2026-08-25 by an implicit-`*%_` retry in
`call_method_with_values`: offer the full argument list first, and only when the
whole chain answers "no such method" retry with the nameds removed. That is
provably non-regressive — a call that succeeds today never takes the retry — and
it closed 24 measured divergences.

**2. The wrong arm hits (silent).** When a positional slot happens to *accept*
the `Pair`, it is numified or consumed as data and the call succeeds with the
wrong answer. There is no error to retry on. A 2026-09-07 re-measurement swept
2 600 (receiver, method, argument-shape) probes derived from
`src/builtins/native_method_row_table.rs`, comparing `R.M(A)` against
`R.M(A, :qqzz9)` in mutsu and then checking every divergence against `raku`:
**754 probes were not named-blind in mutsu**, spanning about 30 methods. A
sample, with the `todo/deep/` file's original six rows marked `*`:

| call | raku | mutsu (before) |
|---|---|---|
| `"abc".chop(:zzz)` * | `"ab"` | `"abc"` (Pair numified to a 0 char count) |
| `10.polymod(3, :zzz)` * | `(1, 3)` | `(1, 3, Inf)` (Pair became a modulus) |
| `3.fmt("%d", :zzz)` * | `"3"` | dies "Too many positionals … got 3" |
| `255.fmt(:zzz)` | `"255"` | `"zzz\tTrue"` (Pair became the format) |
| `(1,2,3).rotor(2, :zzz)` * | `((1, 2),)` | `((1, 2), ())` |
| `(1,2,3).classify({$_}, :zzz)` * | 3 keys | 4 keys |
| `(1,2,3).categorize({$_}, :zzz)` | 3 keys | 4 keys |
| `(1,2,3).first(:zzz)` * | `X::Adverb` Failure | `Any` |
| `(1,2,3).minmax(:zzz)` | `1..3` | `Nil` |
| `"%s".sprintf("x", :zzz)` | `"x"` | dies (surplus sprintf argument) |
| `(1,2,3).AT-POS(1, :zzz)` | `2` | `Failure` "Index out of range" |
| `(1,2,3).EXISTS-POS(1, :zzz)` | `True` | `False` |
| `Buf.new(1,2,3).subbuf(1, :zzz)` | `Buf.new(2,3)` | `Buf.new()` |
| `255.base(16, :zzz)` | `"FF"` | (agreed) |
| `3.expmod(2, 5, :zzz)` | `4` | (agreed on this shape, not on others) |

Two rows of the recorded table had already **drifted** before this work started,
which is why the re-measurement came first: `3.fmt("%d", :zzz)` no longer died
with `X::AdHoc` about a surplus sprintf argument but with a positional-arity
error, and the sweep found roughly five times as many affected methods as the
six the file listed.

The measurement also corrected the file's framing in one important way. It said
`chop` / `polymod` / `fmt` "have no named parsing at all to extend", implying
they are structurally different from `rotor` / `classify`. They are not: what
distinguishes them is only *which* names they accept (none, versus `:partial`
and `:as`/`:into`). And `classify`'s "a named-flavour `Pair` can legitimately
arrive as data" caveat turns out not to apply to the *argument list* at all —
`callable_item`'s comment is about `Pair`s that arrive as **elements of the
list being classified** (from a `Hash`/`Bag` invocant iterating to pairs), which
never pass through the argument vector. A `Pair` genuinely passed positionally
(`rotor(2 => -1)`) carries the *positional* flavour, which ADR-0021 already
keeps distinct.

## Decision

**A builtin method declares the set of named arguments it accepts, and the
builtin dispatch layer drops every named argument outside that set before it
chooses an arity.**

The declaration is `native_method_accepted_nameds(method) -> Option<&'static
[&'static str]>` in `src/builtins/accepted_nameds.rs`, and it is deliberately
**partial**:

- `Some(names)` — surveyed. An undeclared named is dropped before dispatch.
- `None` — not surveyed. Nothing is dropped; behaviour is byte-for-byte today's.

It is consulted at the three places the builtin layer is entered, and **only**
there, so a user-defined method, a constructor and every `%_`-carrying handler
downstream still see the call's own arguments:

1. `vm/vm_native_dispatch.rs::try_native_method_raw`, immediately before the
   `args.len()` cascade and after the existing `native_*_with_options`
   interceptors;
2. `runtime/methods_call_dispatch.rs`, before the interpreter-side twin of that
   cascade;
3. `runtime/methods_call_dispatch.rs`, before `dispatch_method_by_name_1/2/3`.

The filtered list is a local; if the builtin layer declines, the original `args`
flow on unchanged.

`grep` and `first` are handled by a *second*, different mechanism and are
deliberately absent from the table: Rakudo does not let their implicit slurpy
swallow an unknown adverb, it **validates** and answers `X::Adverb` (`grep`
throws, `first` `fail`s). mutsu already did that for `grep`; this ADR extends it
to `first`, in both the method and the sub form.

### Which polarity, and why

Two polarities were available, and the choice is the whole decision:

- **Listed = filtered** (chosen). An omission from the table leaves an already
  wrong answer wrong. It can never invent a new one.
- **Unlisted = filtered** ("the cascade is named-blind by construction"). An
  omission silently deletes a *real* adverb.

The `todo/deep/` file flagged the completeness of the table as the risk of this
design, and it is right — but the risk is a function of the polarity, not of the
table. With "listed = filtered", the failure mode of an *incomplete* table is
"one more known bug stays open", and the failure mode of a *wrong* row (a method
declared that really does read an adverb) is loud: that adverb stops working,
which `make test` and roast see immediately. That asymmetry is what makes the
table safe to grow one row at a time.

### The table is generated, not guessed

`scripts/native-method-adverb-survey.raku` reports, for each method name, the
named parameters every candidate declares across every owner type. That is the
authoritative accepted-name set, because Rakudo's implicit `*%_` means a name
outside it *cannot* change the answer. It also flags the `*%_` slurpy explicitly,
so a routine that validates its own `%_` (`grep`, `first`) is visibly not
answerable by signature alone and gets the validation treatment instead.

`t/native-method-accepted-nameds.t` pins both halves of every declaration — an
undeclared named is invisible, and every declared adverb still reaches the
implementation. **The whole file passes unmodified under `raku` as well as under
mutsu**, so it is a conformance test rather than a snapshot of mutsu's own
behaviour.

## Alternatives considered

### Option 2 — extend the interceptor pattern until no cascade arm reads an argument `Pair`

The `todo/deep/` file's second candidate: lift every adverb-aware native out in
front of the arity cascade (as `native_contains_with_options`,
`native_prefix_suffix_with_options` and `native_substr_eq_with_options` already
are), then make the cascade named-blind by construction. The invariant would be
structural rather than a list to keep in sync, which is genuinely more
attractive.

Rejected on the measurement, for two reasons.

**It does not reach the affected population.** The `arity_bits` column of
`native_method_row_table.rs` says which methods the pure cascade actually
serves. Of the methods this bug touches, only about half are cascade-served
(`chop`, `fmt`, `base`, `expmod`, `AT-POS`, `EXISTS-POS`, `join`, `tail`,
`combinations`, `subbuf`, `int-bounds`, `indent`, `samecase`, `uniprops`); the
rest — `polymod`, `rotor`, `classify`, `categorize`, `first`, `minmax`, `skip`,
`sprintf` — carry `N` (slow path) and are served by `dispatch_method_by_name_*`,
where "no arm reads an argument `Pair`" is not a property one can establish
without rewriting those handlers. Making the *cascade* named-blind would close
half the finding and leave the louder half (`polymod`, `classify`, `rotor`)
open.

**The structural invariant it buys is weaker than it looks.** The cascade is
already almost named-blind — a grep of `methods_0arg/` and `methods_narg/` finds
only five arg-side named reads (`Str(:superscript/:subscript)`,
`lines(:chomp/:count)`, `batch(:elems/:batch)`, `comb`, `split`, plus
`flatten(:hammer)`); nearly every `ValueView::Pair` match in that tree is on the
*receiver* (`.key`, `.value`, `.antipair`, `.raku` of a `Pair`). So option 2's
end state is a five-entry exemption list guarding a filter — which is option 1
with a different name and a much larger diff.

The interceptor pattern is not being retired: it stays where it belongs, for
natives that need `&mut self` or a shape the arity cascade cannot express. It is
just not the mechanism for "this method reads no adverbs at all".

### Strip named arguments unconditionally before dispatch

Already rejected by the 2026-08 work, for the reason recorded there: a native
that genuinely reads an adverb reads it out of that same argument list, so a
blanket strip silently drops real adverbs (`.split(:skip-empty)`,
`.substr-eq(:i)`, `.comb(:match)`, `.subst(:g)`). That is exactly the
"unlisted = filtered" polarity above.

### Reverse the retry (positionals first, full list as fallback)

Also already rejected: `"a,b,,c".split(",", :skip-empty)` would hit the
positional-only `split(",")` arm and lose `:skip-empty` before the fallback ever
ran.

## Consequences

- 332 of the 754 non-named-blind probes become named-blind, including every row
  of the original finding. No probe that was named-blind before stops being so;
  the only new divergences in the sweep are `first`'s intended `X::Adverb` and
  pre-existing QuantHash iteration-order noise.
- The remaining 422 probes are undeclared methods (`None`) plus receiver shapes
  where the plain call already diverges from raku for unrelated reasons (e.g.
  `4.roots(2)` answers `Num`s where raku answers `Complex`). They are residue,
  not regressions, and are tracked in the narrowed
  `todo/deep/native-method-accepted-named-declarations.md`.
- `base` is declared with `:no-trailing-zeroes`, which mutsu does not yet
  implement. Declaring it is still correct — it keeps the adverb reaching the
  implementation, so implementing it later needs no change here.
- Cost on the hot path is one tag-level `is_string_pair_value` scan of the
  argument slice (no `view()`, so a lazy `Match` argument is not materialized),
  and no allocation unless something is actually dropped.
- Dropping an argument shifts the indices a handler uses to read the call site's
  `arg_sources` metadata. Only `classify`/`categorize` do that (to recover the
  variable name behind `:into`), and only when an *undeclared* named precedes
  `:into` in the same call; the mis-indexed source cannot start with `into=`, so
  the lookup falls through to the existing `find_var_by_identity` fallback rather
  than resolving the wrong variable.

## Implementation status

- **Slice 1 (implemented 2026-09-07).** `accepted_nameds.rs` with 29 surveyed
  methods, the three call sites, `first`'s `X::Adverb` validation in both forms,
  `scripts/native-method-adverb-survey.raku`, and
  `t/native-method-accepted-nameds.t` (66 assertions, green under raku and
  mutsu).
- **Slice 2 (implemented 2026-09-07).** 35 more `&[]` rows plus six with a
  non-empty accepted set, `native_base_with_options` (the `base`
  `:no-trailing-zeroes` adverb this ADR listed as declared-but-unimplemented),
  and `scripts/native-method-adverb-sweep.raku` — the sweep this ADR describes,
  now a committed script that runs under any interpreter. See
  `news/2026-09/native-method-accepted-nameds-slice-2.md`.

  Slice 2 corrected two of this ADR's own claims, both by measurement:

  1. **"the adverb reaches the implementation" for `base` is false.**
     `.base($radix, $digits, :no-trailing-zeroes)` is a three-argument call that
     matches no arity arm, so the implicit-`*%_` retry dropped the adverb before
     the 2-ary arm saw it. It needed an interceptor, not just a row.
  2. **The builtin layer has seven entries, not three.** The Decision section
     lists the arity cascade, its interpreter twin and
     `dispatch_method_by_name_1/2/3`. Six further sites dispatch a builtin
     *before* the cascade and had to be routed through the same declaration:
     `vm_baghash_mutators::apply_baghash_mutator`, the `tail` interceptor, the
     by-value array/hash mutator blocks, `exec_call_method_mut_op_impl` +
     `call_method_mut_with_values`, `call_native_instance_method` +
     `try_io_path_lexical`, and the compose-a-method-over-a-callable last
     resort. The compiler's `ArrayPush` fast path also had to decline a named
     call site. Each is restricted to a receiver that cannot be a user class,
     so the "a user method still sees its own nameds" invariant holds.

  It also refuted the tempting shortcut that a method whose only named slurpy is
  the implicit `%_` accepts nothing: `Str.trans` declares only `*%_` and reads
  `:d`/`:s`/`:c` out of it. The survey now reports each slurpy **by name**
  (`(+*%_)` vs `(+*%options)`), which makes the *declared*-slurpy readers
  visible, but a `--` row still has to be confirmed behaviourally.

- **Slice 3 (open).** `subst` / `trans`, `new`, and the plain-call divergences
  the sweep surfaces. Recorded in the narrowed `todo/deep/` file.
