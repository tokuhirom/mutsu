# A same-named `my %h` in another block no longer cancels `is default(...)`

```raku
{ my $z = (my %u is default(42)); say "A ", $z<nope>; }
{ my %u; say "B ", %u.elems; }
```

rakudo prints `A 42` / `B 0`; mutsu printed `A (Any)` / `B 0`. The two blocks
are disjoint lexical scopes and neither runs before the other, and reversing
them changed nothing — both orders lost the default, which is what marked this
as a *compile-time* interaction rather than a runtime one.

## Narrowed by measurement

- Only a **bare** later declaration was needed, but it had to be **used**:
  `{ my %u; }` alone was harmless, `{ my %u; %u.elems; }` triggered it. Using
  the variable is what makes the compiler give it a local slot.
- `@` behaved exactly like `%`; a `$` scalar was unaffected (different
  compilation path).
- `is SetHash` was unaffected, so this was specific to the container's embedded
  default rather than to trait application generally.
- Moving the sibling declaration into a **sub** — its own `locals` list — made
  it go away, while nesting it in another bare block did not.

That last row is the whole story, and `--dump-bytecode` shows it directly: the
two programs compile to the *same* mainline ops for the first block; the only
difference is that `code.locals` gains a `"%u"` entry for the sibling block's
slot.

## Root cause

The expression-position declaration stores its container with `SetGlobal` and
then applies the trait with `ApplyVarTrait { slot: None }` — no compile-time
slot, because the declaration shadows nothing. With no slot, the VM's
`read_var_trait_target` falls back to a **by-name search of `code.locals`**, and
that list is shared by every bare block at the same level. So it found the
*sibling* block's `%u` slot, which is still uninitialized at that point, tagged
that `Nil` with the default, and stored it back — over the env binding the
expression's read-back (`GetHashVar`) then reads. `$z` therefore received a
tagged `Nil` instead of the defaulted hash.

## The fix

A slot value that is not a container is never this declaration's container, so
the slot-resolved read is now filtered on actually being an `Array`/`Hash`
before it wins over the env value. The by-name fallback stays for the cases it
exists to serve; it just cannot hijack an unrelated, unfilled slot any more.

`t/container-default-survives-sibling-declaration.t` pins both orders, both
sigils and the `is SetHash` control, and passes under rakudo as well as mutsu.

The underlying sharpness — that `code.locals` is name-keyed and shared across
sibling bare blocks, so any by-name slot search can cross a scope boundary —
is unchanged and remains part of the locals/env dual-store debt.

Closes [#7621](https://github.com/tokuhirom/mutsu/issues/7621).
