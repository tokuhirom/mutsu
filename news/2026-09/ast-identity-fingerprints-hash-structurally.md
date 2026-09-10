# AST identity fingerprints hash the AST instead of its `Debug` rendering

`src/ast.rs` identified a routine by streaming a `Debug` *rendering* of its AST
into a hasher:

```rust
let mut sink = HashWrite(&mut hasher);
let _ = write!(sink, "{params:?}\x00{param_defs:?}\x00{body:?}");
```

`registration_identity_fingerprint` and `sub_registration_fingerprint` had the
same shape, and roughly fourteen call sites across the compiler, the VM and the
runtime reach one of them — so essentially every sub, closure and method body
the compiler touched dragged a full `core::fmt` traversal of its AST behind it.
`callgrind` on Cro's HTTP/2 DATA frame once put `<&T as core::fmt::Debug>::fmt`
at 78.6% of the frame ([#7822](https://github.com/tokuhirom/mutsu/issues/7822),
split out of #7667). Two rounds of fixes to the *runtime recompiles* that made
it that visible (#7801, #7812) shrank the share, and an earlier pass replaced
three `format!` allocations with the `HashWrite` sink
(`news/2026-08/routine-identity-fingerprint-is-memoized-on-the-def.md`) — but
the formatting machinery itself was still what every remaining fingerprint paid.

The fix is the obvious one, and the obstacle was never the fingerprints: it was
that `Expr` holds a `Value`, so hashing structurally needs `Value: Hash`.

## `Hash for Value` is a declaration-identity hash

`src/value/identity_hash.rs` implements it, with the contract written down in
the module docs so nobody later reuses it for value equality:

- It is deliberately **inconsistent** with `PartialEq for Value`, which is
  Raku's `eqv`-flavoured value equality — that impl makes `Int(1) == Num(1.0)`
  and `Array([1]) == Seq([1])`, and this hash separates them. A fingerprint
  answers "were these two literals parsed from the same source shape", so
  hashing `NaN` by bit pattern and separating `0.0` from `-0.0` is *correct*
  here rather than a compromise.
- `Value` must never gain an `Eq` impl. `HashMap`/`HashSet` require
  `K: Eq + Hash`, so the missing `Eq` is what mechanically stops anyone from
  keying a map on this hash and silently getting Raku's value equality wrong.

The variants a parser can actually plant in an AST — the numeric and string
immediates, ranges, pairs, versions, `Nil`/`*`/`**` — hash structurally. The
live-object variants (`Sub`, `Instance`, `Promise`, `Proxy`, the `Gc`-backed
containers) keep the old `Debug`-into-the-hasher fallback: they are unreachable
from a literal in practice, and several of them carry a per-object `WhichId`
that is not structurally comparable across two parses anyway, so the fallback
is the honest answer for them rather than a shortcut.

Everything else is `#[derive(Hash)]` on the AST types, plus a hand-written
`Hash for TokenKind` — needed only because two of its variants carry an `f64`;
`mem::discriminant` covers its ~110 unit variants.

## The two properties that had to survive

- `registration_identity_fingerprint` is deliberately line-insensitive (it
  filters top-level `Stmt::SetLine`) while `function_body_fingerprint` is not.
  Both keep their behaviour; an explicit statement-count hash stands in for the
  length prefix a slice hash would have written.
- Fingerprints are compared across separately-parsed copies of one source, so
  the hash depends on structure alone — no addresses, no ids that vary per run.
  `Symbol` hashes an interning id, which is a pure function of the symbol's text
  within a process, and no fingerprint is ever serialized
  (`FunctionDef::body_fp_cache` is `#[serde(skip)]` and
  `CompiledRoutineMetadata` is in-process only).

`src/ast/fingerprint_tests.rs` pins both, along with the field-separation and
identity-vs-value-equality contracts.

## Measured

`callgrind` on an `EVAL` loop that compiles a fresh non-trivial routine per
iteration (`Ir`, inclusive):

| | before | after |
| --- | --- | --- |
| `function_body_fingerprint` | 56,209,000 (4.17%) | 13,963,000 (1.08%) |
| `registration_identity_fingerprint` | 22,046,400 (1.64%) | 5,451,800 (0.42%) |
| `sub_registration_fingerprint` | 22,552,200 (1.67%) | 5,653,800 (0.44%) |
| program total | 1,346,551,496 | 1,287,832,901 |

Each fingerprint costs about a quarter of what it did; `core::fmt`'s
`DebugStruct::field` / `str as Debug` / `HashWrite::write_str` entries disappear
from the profile entirely, and `sip::Hasher::write` drops from 99.7M to 70.6M
`Ir` because it is no longer fed a character at a time. The whole program is
4.4% fewer instructions on that workload, and 5.24s → 4.85s of wall clock
(medians of five) on a longer run of it. A workload that compiles more at
runtime gains proportionally more.
