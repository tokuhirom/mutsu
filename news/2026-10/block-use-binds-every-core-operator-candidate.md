# A block's `use` binds the candidates of every core operator lexically

ADR-8751 stage 2 bound a block-scoped import's user candidates of the
arithmetic, comparison, string and set operators in the block's lexical
import set. The other core operators still left theirs in the registry, or
bound them without the core operator behind them:

- `{ use M; 1 .. 3 }`, where `M` exports `multi infix:<..>(Foo, Foo)`, died
  with "Cannot resolve caller": the import was taken for a user operator,
  so the bound dispatcher had no core `..` to fall back to. The same held
  for `^..^`, `but`, `unicmp`, `=~=` and the prefix `^`.
- An imported `~^`, `~|` or `minmax` candidate reached a routine written
  outside the block while the block ran.
- An imported `=>`, `%%`, `+&` or `+<` candidate was ignored inside the
  block.

Now the bitwise operators, the ranges, `...`, `<=>`, `===`, `=~=`, `=>`, the
junction constructors, `before`/`after`, `but`, `coll`, `unicmp`, `minmax`
and `o` bind their imported candidates in the set. Their ops ask no user
candidate, so in such a block the compiler puts `LexImportInfix` in front of
the op: the bound dispatcher replaces it, and falls back to the core operator
for operands no candidate accepts. The prefixes `^`, `+^`, `~^` and `?^` join
the core prefixes a dispatcher falls back to, which also fixes `^3` after a
program's own `multi prefix:<^>(Foo)` ("Unknown prefix operator").

A bare-file module's multi family (no `unit module`) binds lexically too;
before, it stayed visible to callees and even after the block.
