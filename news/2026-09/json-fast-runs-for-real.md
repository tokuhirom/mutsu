# Upstream `JSON::Fast` runs: the recorded blocker was nine `nqp::` ops, not fifty

`docs/batteries/json-tiny.md`, `BATTERIES.md`, `src/runtime/json.rs`'s header,
`src/vm/vm_native_json.rs`, `ANALYSIS.md` and
`news/2026-07/nqp-op-layer-measured-and-rejected.md` all said the same thing
about why mutsu ships a native `to-json`/`from-json` instead of the real
distribution:

> the real `JSON::Fast` depends on ~50 `nqp::` ops mutsu does not implement

Nobody had run it. Probed op by op against upstream
`JSON::Fast:ver<0.20.1>:auth<zef:timo>` (1117 lines, Artistic-2.0, no runtime
dependencies), **42 of its 51 `nqp::` ops already worked**. The module parsed
completely and reached *runtime*, dying on the first of nine missing ops. The
2026-07 entry that produced the "42 missing" figure was measuring a tree that no
longer existed: its own premise — "mutsu already ships its own JSON::Fast … so
the real distribution never runs" — was removed by #8203, and its verification
repro ("dies with `Unknown function: list_i`") stopped reproducing months ago.

This is step 2 of [#8226](https://github.com/tokuhirom/mutsu/issues/8226), and it
closes [#8215](https://github.com/tokuhirom/mutsu/issues/8215) (13 zef
distributions blocked on three of the nine) outright. **13 of the 14 upstream
`JSON::Fast` test files now pass whole against the real distribution.**

## The nine ops

`nqp::bindpos`, `nqp::shift_i`, `nqp::pop_s`, `nqp::push`, `nqp::chr`,
`nqp::p6scalarwithvalue`, `nqp::p6bindattrinvres`, `nqp::hash` and
`nqp::ifnull`. They live in a new `src/runtime/nqp_ops_list.rs` — the untyped
twins of the typed native-list ops `nqp_ops_text.rs` already held, plus the queue
ends and the two `p6*` bridges — with their natural siblings (`pop`, `pop_i`,
`pop_n`, `shift`, `shift_s`, `shift_n`) implemented alongside rather than waiting
for the next module to ask.

`nqp::ifnull` is a **compiler special form** (`compiler/nqp_forms.rs`), not a
builtin, because its second operand must not be evaluated when the first is
there: rakudo's idiom is
`nqp::ifnull(nqp::getattr(…), nqp::bindattr(…, fresh))`, which would install a
fresh empty store over a live one if both arms ran.

## What the nine were hiding

The issue's own risk list said "nine ops is the *first* stop, not a proof of
completion". It was right; four more things had to be true.

**A `Uni` is its codepoints.** rakudo declares `Uni` `is repr('VMArray')
is array_type(uint32)`, and `JSON::Fast` treats one as exactly that: its escaper
takes a string's `.NFD` and rewrites it in place with `nqp::splice`, and its
string scanner consumes one with `nqp::shift_i`. mutsu stored a Uni as the
normalized *string*, which left it with no element store at all — so
`nqp::elems` answered 0, the escaper's scan loop never ran a single iteration,
and `to-json` **silently emitted unescaped, invalid JSON** for any string
containing a quote or a control character. `UniData` now holds its codepoints in
a shared array and derives the string (`UniData::text()`), which also makes two
holders of one Uni share its store the way two references to a VMArray do.

**`nqp::create` has to allocate storage.** A `Map`/`Hash`/`List`/`Array`, a class
declared `is repr('VMHash')` / `is repr('VMArray')`, and a `Uni` are all, in
mutsu, indistinguishable from their own store; `Mu.CREATE`'s attribute-less
instance is something `nqp::bindkey` and `nqp::push` cannot reach. Each now comes
back as an empty store. The two VM reprs are tracked in the class registry beside
the existing `CStruct`/`CUnion`/`CPointer` sets.

**Installing a storage object unifies two stores.** rakudo's List and Map wrap a
*separate* storage object that nqp code installs through `'$!reified'` /
`'$!storage'`. mutsu has no wrapper — which `nqp_attr_value` already knew, since
a `'$!storage'` *read* on a Hash hands the hash itself back — so installing means
the container takes the storage's current contents **and** the storage object is
re-pointed at the container's node. Both halves are load-bearing because nqp code
does it in both orders: `hllize-list` fills a buffer and then installs it, while
`parse-array` installs an empty one and only then pushes onto it.

**`nqp::strfromcodes` normalizes.** A VM string is NFG, so rakudo's
`nqp::strfromcodes("bå".NFD)` is the *composed* two graphemes. Without that,
every string `JSON::Fast` round-tripped through `.NFD` and back came out
decomposed.

## A general bug found on the way: a parameter default could eat a routine's `&` lexicals

`t/08-sorted-keys.t` failed with `Unknown function: to-json` — for a call that
worked fine one scope out. Reduced to nothing module-shaped at all:

```raku
my &f = -> $n { say "f $n" };
sub h($x, @k = $x.keys) { f(4) }
h({a => 1});              # mutsu: Unknown function: f      raku: f 4
```

A parameter default is evaluated re-entrantly, and that carrier snapshots the
env's code-var entries on entry so a block-local `sub` / `my &f` cannot leak out
of it. The snapshot only sees the current tier's overlay — but a nested call
inside the default can *flatten* the env chain and migrate the caller's own
`&`-bindings into that tier, and the restore then dropped them as block
additions, with no parent tier left for them to shadow back through. So **any**
routine with a default that had to be computed re-entrantly (anything beyond a
literal) lost every `&` lexical its body could see.

It hit `use JSON::Fast` hardest because its `EXPORT` sub hands `&to-json` over as
a pure lexical rather than a package symbol, so there was no package fallback to
mask the loss. The restore now puts a binding back unless the block's own
compiled chunk is what declared the name — which keeps the leak it exists to
prevent prevented.

## What is left, and what it is not

`t/01-parse.t` is the one upstream file still failing, on two blockers that have
nothing to do with JSON:

- `Q«[{"":» x 10_000` recurses ~20,000 routines deep. mutsu aborts the process on
  a Rust stack overflow rather than raising a catchable error; its ceiling is
  between 5,000 and 10,000 frames, where rakudo raises a catchable `X::AdHoc`.
  Filed as [#8232](https://github.com/tokuhirom/mutsu/issues/8232).
- `nom-comment($text, ++$pos)` passes `++$pos` to an `int $pos is rw` parameter.
  rakudo binds the native reference; mutsu refuses with "expects a writable
  container", so the comment scanner's `--$pos` never reaches the caller and
  `from-json('{"a":"b"}/')` accepts trailing garbage. Filed as
  [#8233](https://github.com/tokuhirom/mutsu/issues/8233).

Vendoring the distribution to `modules/JSON-Fast/`, deleting
`src/runtime/json.rs` + `src/vm/vm_native_json.rs` (~1,100 lines) and closing
ADR-0096 §D4's last unjustified rung-3 entry are sequenced behind those two. The
ADR row is re-decided as *scheduled for retirement* rather than left citing an
expired rationale, which is what D4 requires.

## Tests

- `t/vm/nqp-list-hash-ops.t` — 35 assertions over the nine ops, their siblings,
  `nqp::ifnull`'s laziness, `nqp::create` of each storage type, and storage
  installation in both orders. Passes identically under `raku`.
- `t/types/string/uni-codepoint-store.t` — a Uni read, consumed, spliced in
  place and shared between two holders, plus `strfromcodes`' normalization.
  Passes identically under `raku`.
- `t/routines/signature/param-default-keeps-code-lexicals.t` — the `&`-lexical
  regression, with the block-local-leak case it must not reintroduce. Passes
  identically under `raku`.
