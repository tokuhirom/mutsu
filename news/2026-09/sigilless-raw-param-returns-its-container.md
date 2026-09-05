# A sigilless parameter's container survives the return

`sub f(\x) is raw { x }; my $a = 42; f($a) = 5` died with `Cannot modify an
immutable Int (42)`. So did the `is rw` spelling, and the explicit `return-rw x`
spelling, and the method form `class C { method m(\x) is rw { x } }`. raku
writes `5` through to `$a` in every one of them.

## What was wrong

ADR-0059 made an `is rw`/`is raw` routine compile its tail in *container mode*,
so the routine hands its caller a storage location rather than a value. The tail
compile recognises the location by asking `scalar_container_alias_name` for the
lexical name an expression denotes the container of — and that function matches
`Expr::Var` only.

A **sigilless** lexical (`\x`) is spelled `Expr::BareWord` by the parser. Same
local slot, same local name (`"x"`), different AST node — so the tail was never
recognised and no cell was captured. The bytecode is the whole story:

```
sub f($x is rw) is rw { $x }    ->  GetLocal(0); WrapVarRef{name_idx:0,slot:0}; CaptureVarCell
sub f(\x)       is rw { x }     ->  GetLocal(0)
sub f(\x) { return-rw x }       ->  GetLocal(0); CallFunc "return-rw"
```

## The fix

`return_rw_container_name` became a method so it can consult `self.local_map`,
and gained a `BareWord` arm. The gate matters: `Expr::BareWord` is also how a
type name, an enum value and a bare call are spelled, so a bareword denotes a
container only when it actually resolves to a local slot of the frame being
compiled — which is precisely what `local_map` records. Guessing from the
spelling would have boxed `Int` in `my @a = (Int, Str)`.

The arm is deliberately scoped to the `return-rw` / rw-tail site and **not**
folded into `scalar_container_alias_name` itself, whose other callers (List
literal elements, fat-arrow Pair values) see type-name barewords in ordinary
code.

## Measured

Verified against raku v2026.07, before and after:

| | raku | before | after |
|---|---|---|---|
| `sub f(\x) is raw { x }; f($a) = 5` | `5` | dies | `5` |
| `sub f(\x) is rw { x }; f($a) = 5` | `5` | dies | `5` |
| `sub f(\x) { return-rw x }; f($a) = 5` | `5` | dies | `5` |
| `f($a).VAR.^name` | `Scalar` | `Int` | `Scalar` |
| `my $b := f($a); $b = 7; say $a` | `7` | dies | `7` |
| `my @a = 1,2; f(@a[0]) = 9` | `[9 2]` | dies | `[9 2]` |
| `class C { method m(\x) is rw { x } }; C.new.m($a) = 5` | `5` | dies | `5` |

Pinned by `t/sigilless-raw-param-container-return.t` — 21 tests, byte-identical
output under `mutsu` and `raku`, including the non-regression rows that
constrain the gate (`my @a = (Int, Str)`, `my $p = (a => Int)`, `my \y = 5`,
`my \z := $v`, and a routine returning an inline sigilless declaration), and the
row that keeps a *non*-rw-capable routine refusing.

## Where this sits

This is Slice 1 of
[ADR-0067](../../docs/adr/0067-a-routine-hands-back-the-container-it-was-given.md),
which is the design that unifies two open findings — a method handing back its
invocant's container (`.snitch = ...`) and an lvalue subscript chain stepping
through an object's `AT-KEY`/`AT-POS`. ADR-0067 corrects three claims those
findings made: `.VAR` is not an acceptance case (raku refuses it too), the
invocant problem is not native-specific (a user `augment` method fails
identically), and the chain failure is silent in four of its six broken
spellings rather than loud in all of them.
