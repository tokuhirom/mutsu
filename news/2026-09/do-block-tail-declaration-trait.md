# A `do` block's tail declaration keeps its variable trait

```raku
my $z = do { my %u is SetHash };
say $z.^name;                      # raku: SetHash   mutsu: Hash

my $y = do { my $s is default(7) };
say $y;                            # raku: 7         mutsu: (Any)
```

Split off from the ticket that fixed the *parenthesised* expression position
(`news/2026-09/inline-container-trait-in-expression-position.md`). This is the
one row of that neighbourhood that turned out to be a different bug: there the
trait ran and the expression merely returned the pre-trait value; here **no
`ApplyVarTrait` op was emitted at all**.

## Root cause

`compile_block_inline` has a hand-inlined `Stmt::VarDecl` arm for the *block-final*
position — it exists so a block-final declaration yields its value, and it is
neither the statement-position `Stmt::VarDecl` path in `stmt.rs` nor the
expression-position one in `expr_block.rs`. It emitted a bare
`SetVarDynamic / MakeHash / Dup / SetGlobal`, never looking at `custom_traits`.

That position needs both halves: a container trait (`is SetHash`, `is BagHash`,
`is Buf`) *replaces* the declared container and `is default(...)` embeds a
default in it, so the trait has to be applied **after** the store and the
container read back — while the arm yielded the value from a `Dup` taken
**before** it.

## The fix

The expression-position path already does exactly that, so a block-final
declaration carrying a non-internal trait routes through `compile_expr_do_stmt`
instead of growing a second copy of the rule. Untraited declarations are
untouched and keep the hand-inlined arm.

Pinned by `t/do-block-tail-declaration-trait.t`, whose 16 assertions pass
unchanged under rakudo (8 of them fail on the unfixed binary):
`is SetHash`/`BagHash`/`MixHash`/`Buf` as the tail, `is default(...)` for all
three sigils, a routine's tail declaration, and four controls that were already
correct. `t/inline-container-trait-expression.t`,
`t/container-capture-cell-dichotomy.t` and
`roast/S02-types/{baghash,mixhash,sethash}.t` are unmoved.

## A side finding, filed separately

Every declaration in the pin uses its own variable name on purpose: a same-named
`my %h` elsewhere in the same file cancels an `is default(...)` container, which
is [#7621](https://github.com/tokuhirom/mutsu/issues/7621) — pre-existing and
independent (it reproduces on unmodified `main` through the parenthesised
expression path, which this change does not touch).

Closes #7583.
