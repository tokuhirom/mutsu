# Call, declaration, module, regex and meta-operator opcodes documented

This is the last slice of the opcode reference (`site/opcodes.html`). It adds
doc comments to the 49 remaining `OpCode` variants in `src/opcode.rs`, which
leaves every one of the 374 opcodes documented. The slice covers:

- call and block-region ops
- declaration and module ops
- `die`/`fail`
- the four output statements
- magic and variable access
- substitution and transliteration
- `take`
- the hyper and meta operators

As #9445 asked, each doc says where the op hands off to the runtime or the
interpreter slow path. For the declaration and module ops it names the
compile-time plan or pool entry the op consumes. `RegisterDecl` indexes
`CompiledCode::decl_plans`, a tagged reference into the
`sub`/`class`/`role`/`proto`/`token` plan tables. `RegisterEnum` and
`RegisterSubset` take a `stmt_pool` entry. For the region ops (`BlockScope`,
`DoBlockExpr`, `OnceExpr`, `PackageScope`, `ReactScope`) the doc spells out the
layout: which ops follow, and where each `*_end` points.

The research turned up several ops whose names suggest more than they do.

- **`RoutineMagic` / `BlockMagic` are unreachable.** The parser no longer
  builds the `Expr` variants they compile from. `&?ROUTINE` and `&?BLOCK`
  compile to `GetCodeVar`.
- **Several reads use a different op than their names suggest.** `$0` is a
  plain `GetGlobal`, not `GetCaptureVar`, which only handles `$<name>`.
  `die`, `fail` and `say` in expression position compile to `CallFunc`.
- **`CallMethodMut` covers every method call on a named variable**, not only
  the mutating ones.
- **`DeleteIndexExpr` is only emitted for `:exists:delete`** on a computed
  container.
- **A failed non-global `s///` pushes `False`, where Rakudo gives `Nil`.** The `Subst` doc records the current behaviour and points at #9515, filed for it.
- **`InfixFunc` ignores the `Z` modifier.** `3 Z[&foo] 4` calls `foo(3, 4)`
  once, where Rakudo zips. The doc records this, and the case was added to
  #9464, the open issue on metaops over code values.
- **`MakeBlockClosure` is rarely reached from source.** A `{ $^a + $^b }`
  term compiles to `MakeAnonSubParams`.

Tracked as #9445.
