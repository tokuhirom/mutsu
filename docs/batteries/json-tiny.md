# Battery: JSON (reference) — `JSON::Tiny`

**Slot:** JSON (reference / `Grammar`+`Actions`) · **Chosen:** `JSON::Tiny`
v1.0 (`moritz/json`, Artistic-2.0) · **Kind:** Adopted (community module,
vendored as-is, and **run as-is**)

## What it is

A minimal, pure-Raku JSON (de)serializer — a grammar, an actions class, and
a thin `to-json`/`from-json` wrapper around them:

```raku
use JSON::Tiny;
my $json = to-json([1, 2, "a third item"]);
my $copy = from-json($json);
```

Three files (~120 lines), zero `nqp::` use, zero dependencies.

## `use JSON::Tiny` runs the vendored module

It resolves through the ordinary precedence chain — `use lib` → `-I` →
`MUTSULIB` → the `mzef` site repo → the bundled floor at
`modules/JSON-Tiny/lib` ([BATTERIES.md §6](../../BATTERIES.md)) — like any
other battery. There is no special case for the name anywhere in the parser,
in `use`-time gating, or in dispatch.

That is a change of policy, made on 2026-09-12
([#8183](https://github.com/tokuhirom/mutsu/issues/8183)). **This record used
to declare the opposite arrangement permanent** — which
[ADR-0096](../adr/0096-batteries-adoption-policy.md) then took out of its hands:
a battery's selection record does not decide policy, and ADR-0096 §E2 recorded
the interception as an exception *scheduled for retirement*. This is that
retirement. The history matters:

- mutsu answered the bare names `JSON::Fast` *and* `JSON::Tiny` from one
  native Rust implementation (`src/runtime/json.rs`), recognized at `use` time
  *before* the module search ran, and returned ahead of `call_routine_def` at
  the dispatch sites so it also beat the vendored module's own resolved
  routines.
- The justification was throughput: 200 META-shaped documents parsed in 0.49s
  natively against **>600s** through the real grammar on mutsu's regex engine.

**Being slow is not a reason to substitute** (ADR-0096 §D3). The distinction
the project holds is between an *optimization* — selected transparently, semantically
indistinguishable from the code it replaces, which is what the JIT does to
bytecode — and a *substitution*, which changes what the program observes. This
one was observable three ways: `to-json([1,2,"x"])` answered a pretty-printed
block where the module answers `[ 1, 2, "x" ]`; a parse failure's exception
type was guessed from *which module names appeared anywhere in the program*
(`json_tiny_exception_style()` was `JSON::Tiny loaded && !JSON::Fast loaded`);
and the whole resolution ladder was bypassed, so an `-I` or site-repo copy —
an upstream security fix, say — could not reach a user who could not rebuild
mutsu. Rung 3 is banned because a module that only *looks* like the upstream
one is a private dialect ([BATTERIES.md §1](../../BATTERIES.md)); that
argument does not weaken when the divergence is bought with a benchmark.

Two facts the original record did not have also turned out to matter:

- **zef's metadata path was never on this.** `Zef::from-json` calls
  `Rakudo::Internals::JSON.from-json` (`vendor/zef/lib/Zef.rakumod:9`) — a
  *core Rakudo class*, which mutsu implements natively as a genuine builtin and
  which this change does not touch. The only `JSON::Fast` mention in zef is
  inside a `=begin pod` block. So the regression this mechanism was held in
  place to prevent did not apply to the path it was named for.
- **The gap is an order of magnitude smaller than recorded.** The same 200-document
  measurement today: **12.6s** through the real grammar (raku: 0.84s). mutsu's
  grammar engine is ~15x off rakudo, not off the scale. That 15x is the real
  bill and is worth paying; it is not a reason to keep a substitution.

## `JSON::Fast` went the same way

`JSON::Fast` is a vendored battery too now — `modules/JSON-Fast/`, upstream tag
0.20.1, all 14 of its own test files passing — and the last-resort native
provider that answered its name is **deleted**
([#8226](https://github.com/tokuhirom/mutsu/issues/8226)). Its own record is
[json-fast.md](json-fast.md); the section below is the history of the blocker
both modules were held behind, which is why it stays here.

### The recorded "~50 missing `nqp::` ops" blocker was wrong, and is gone

Every earlier record here — and in `BATTERIES.md`, `src/runtime/json.rs`'s
header, and `news/2026-07/nqp-op-layer-measured-and-rejected.md` — said the real
distribution "depends on ~50 `nqp::` ops mutsu does not implement". Probed op by
op against upstream `JSON::Fast:ver<0.20.1>` in September 2026, **42 of its 51
`nqp::` ops already worked**; the missing nine were `bindpos`, `shift_i`,
`pop_s`, `push`, `chr`, `p6scalarwithvalue`, `p6bindattrinvres`, `hash` and
`ifnull` ([#8226](https://github.com/tokuhirom/mutsu/issues/8226)).

Those nine are implemented, and so is what they turned out to be hiding:

- **`Uni` is now a codepoint store.** rakudo declares `Uni` `is repr('VMArray')
  is array_type(uint32)`, and `JSON::Fast` treats one as exactly that — it
  consumes a string's `.NFD` with `nqp::shift_i` and rewrites it in place with
  `nqp::splice`. mutsu stored a Uni as the normalized *string*, which left it
  with no element store at all, so `nqp::elems` answered 0 and the escaper's
  scan loop never ran: `to-json` emitted *unescaped*, invalid JSON for any
  string containing a quote or a control character. `UniData` now holds its
  codepoints in a shared array and derives the string.
- **`nqp::create` allocates storage.** A `Map`/`Hash`/`List`/`Array`, an
  `is repr('VMHash')`/`is repr('VMArray')` class, and a `Uni` are all, in mutsu,
  indistinguishable from their own store, so `CREATE`'s attribute-less instance
  was unusable; each now comes back as an empty store that `nqp::bindkey` /
  `nqp::push` can build.
- **`'$!reified'` / `'$!storage'` installs unify two stores.** rakudo's List and
  Map wrap a separate storage object that nqp code installs into them; mutsu has
  no wrapper, so installing means the container takes the storage's contents
  *and* the storage object is re-pointed at the container's node — nqp code does
  it in both orders (`hllize-list` fills then installs; `parse-array` installs
  then fills).
- **`nqp::strfromcodes` normalizes.** A VM string is NFG, so rakudo's
  `nqp::strfromcodes("bå".NFD)` is the *composed* two graphemes. Without that,
  every string `JSON::Fast` round-tripped through `.NFD` came back decomposed.

With those in place, 13 of the 14 upstream `JSON::Fast` test files passed. The
last one, `t/01-parse.t`, was held by two blockers with nothing to do with JSON
— a ~20,000-frame recursion that aborted the process on a Rust stack overflow
instead of raising ([#8232](https://github.com/tokuhirom/mutsu/issues/8232)) and
`++$pos` passed to an `int $pos is rw` parameter
([#8233](https://github.com/tokuhirom/mutsu/issues/8233)) — plus three more
general gaps found when the whole suite was re-measured
([#8282](https://github.com/tokuhirom/mutsu/issues/8282)). All are fixed, all 14
files pass, the distribution is vendored, and the ADR-0096 §D4 ledger row is
closed.

## Upstream test suite

All 6 upstream files pass whole against the bundled `lib/` and are on
`batteries-whitelist.txt` (`t/04-roundtrip.t` has 10 expected `TODO passed`).

`t/01-parse.t` was the last to get there. It sat at **92/93** from the
interception retirement until 2026-09-13, on this assertion:

```raku
throws-like {
    use JSON::Tiny;
    from-json '',
}, X::JSON::Tiny::Invalid;
```

Both of `throws-like`'s arguments evaluate before the block is invoked, so
`X::JSON::Tiny::Invalid` was read before the block's `use` had run. Raku
performs `use` at BEGIN time and has the symbol; mutsu's `use` was a runtime
opcode, so the reference got a fabricated stub and the type comparison failed
against the module's real `JSON::Tiny::X::JSON::Tiny::Invalid`. That was a
general `use`-is-not-BEGIN-time gap with a JSON-free repro, filed as
[#8201](https://github.com/tokuhirom/mutsu/issues/8201) and fixed by hoisting
the *load* half of a nested `use` to the head of the compunit — see
`news/2026-09/use-in-a-nested-block-loads-at-begin-time.md`.

The assertion did pass under the old arrangement — but only because both sides
were mutsu fabrications agreeing with each other: the native path threw an
exception named `X::JSON::Tiny::Invalid`, and the parser pre-registered that
bare name as a user type. Retiring the interception did not break it so much
as stop hiding it.

Local pins: `t/modules/batteries/json-tiny-compat.t` (48 assertions, now run
against the bundled module rather than skipped),
`t/exceptions/json-tiny-invalid-exception.t`,
`t/modules/batteries/json-module-ladder.t`.

## Provenance and update procedure

Per [BATTERIES.md §3](../../BATTERIES.md#updating-a-vendored-module-must-be-documented-per-library).
To bump the module, re-vendor — do **not** hand-edit the vendored tree:

| Module | Upstream | Pinned version | Commit |
| --- | --- | --- | --- |
| `JSON::Tiny` | <https://github.com/moritz/json> | v1.0 | `a5ef8c17` (2017-10-24) |

What is vendored: `lib/` plus `META6.json` and `README.md` for attribution
(upstream ships no separate `LICENSE` file; the README carries the license
statement, same situation as `Crypt::Random`). Upstream `t/` and CI config
are excluded — the release gate fetches the tests fresh at the pinned commit.

```sh
rsync -a --exclude '.precomp' <checkout>/lib/ modules/JSON-Tiny/lib/
cp <checkout>/{META6.json,README.md} modules/JSON-Tiny/
# then bump batteries.lock, re-run the gate, refresh the Pages manifest:
cargo build --release && scripts/battery-testsuite.sh --update
git diff batteries-whitelist.txt
python3 scripts/gen-batteries-manifest.py
```

Verification after a bump:

```sh
mutsu -e 'use JSON::Tiny; say to-json([1, 2, "x"])'              # [ 1, 2, "x" ]
mutsu -e 'use JSON::Tiny; try { from-json "" }; say $!.^name'    # JSON::Tiny::X::JSON::Tiny::Invalid
mutsu -I modules/JSON-Tiny/lib -e 'use JSON::Tiny::Grammar; say JSON::Tiny::Grammar.parse(q<{"a":1}>).defined'   # True
```

## License

**Artistic-2.0** — stated in `META6.json`'s `license` key and restated in the
upstream README ("can be used, modified and redistributed under the terms of
the Artistic License Version 2"). Vendored verbatim with `META6.json` /
`README` preserved for attribution, source unmodified (per
[BATTERIES.md §4](../../BATTERIES.md#4-license-policy)).
