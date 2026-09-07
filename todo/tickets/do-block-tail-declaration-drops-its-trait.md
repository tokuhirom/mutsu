# A declaration that is a `do` block's tail statement drops its variable trait

Split off 2026-09-07 from
`todo/tickets/inline-container-trait-declaration-in-an-expression-is-a-plain-hash.md`
(`news/2026-09/inline-container-trait-in-expression-position.md`), which fixed
the parenthesised expression position. This is the one row of that ticket's
neighbourhood list that turned out to be a *different* bug: there the trait ran
and the expression merely returned the pre-trait value; here **no
`ApplyVarTrait` op is emitted at all**.

## Repro

```raku
my $z = do { my %u is SetHash };
say $z.^name;                      # raku: SetHash   mutsu: Hash
```

It is not specific to container traits — `is default(...)` is lost the same
way, for both sigils, which is what shows the trait is never applied rather
than applied too late:

```raku
my $z = do { my %u is default(42) };
say $z<nope>;                      # raku: 42   mutsu: (Any)

my $y = do { my $s is default(7) };
say $y;                            # raku: 7    mutsu: (Any)
```

Declared as an ordinary statement inside the same block, everything agrees:
`do { my %u is SetHash; %u.^name }` is `SetHash` in both, and so are
`{ my %v is SetHash; say %v.^name }` and `sub f { my %w is SetHash; %w }`.

## Narrowed

`--dump-bytecode` on the repro shows the whole block body as
`SetVarDynamic { %u } / MakeHash(0) / Dup / SetGlobal(%u)` — no
`ApplyVarTrait`, and none of the marker/read-back shape the parenthesised
`(my %u is SetHash)` path emits (`MarkVarDeclContext / SetGlobal / GetHashVar /
Dup / SetLocal / ... / ApplyVarTrait`). So the block's TAIL statement is
compiled by neither the statement-position `Stmt::VarDecl` path in
`src/compiler/stmt.rs` (which emits the trait) nor the expression-position one
in `src/compiler/expr_block.rs` (which now emits it before the read-back) — it
takes a third route that never looks at `custom_traits`.

## Where to look

Find which site compiles the last statement of a `DoBlockExpr` body: the
`Dup`-then-`SetGlobal` shape is the giveaway (`compile_block_inline` and its
tail handling). It should reuse the expression-position `Stmt::VarDecl` arm in
`expr_block.rs`, which already applies the traits and reads the container back,
rather than emitting a bare store.

## Check when fixing

The two repros above; `is BagHash` / `is MixHash` / `is Buf`; the same
declaration as the tail of a bare block used as a value (`sub f { my %w is
SetHash }`); a trait taking an argument (`is default(42)`) as the tail, whose
argument has to be compiled before the trait op; and the pins
`t/inline-container-trait-expression.t`,
`t/container-capture-cell-dichotomy.t` and
`roast/S02-types/{baghash,mixhash,sethash}.t`.
