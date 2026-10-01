# A typed `our` variable's constraint holds on every access path

After typed `our` declarations were enforced by name and through `$Pkg::v`
(#10410), four other ways of reaching the same container still skipped the
declared type, where Rakudo enforces it because the constraint lives on the
container (#10411):

```raku
package P { our Int @a = 1, 2; our Int %h = a => 1; our Int $v = 1 }
@P::a[5] = "x";             # now X::TypeCheck::Assignment
%P::h<b> = "x";             # now X::TypeCheck::Assignment
P::<$v> = "a";              # now X::TypeCheck::Assignment
my $r := $P::v; $r = "a";   # now X::TypeCheck::Assignment
```

- **Element and slice stores** read their element constraint from the by-name
  type lane only, which a package-qualified name never has. They now fall back
  to the element type the container itself carries (`Array[Int]`,
  `Hash[Int]`), via the new `element_store_constraint`. The same fallback makes
  an element store into a typed `Array` passed to an untyped `@` parameter
  checked, as in Rakudo.
- **Stash assignment** `Pkg::<$v> = ...` (and `:=`) fell through to the
  generic index-assign, which wrote into a throwaway stash hash: the store was
  silently dropped even for an untyped variable. A literal `$` key is now
  compiled as the `$Pkg::v` assignment it spells, which writes through the
  variable's cell.
- **A scalar bound to a qualified name** (`my $r := $P::v`) became a by-name
  alias whose writes replaced the package entry with the raw value, discarding
  the cell and its constraint for good (and a write through the alias from
  inside a routine was lost). It now joins the source's existing shared cell,
  the same promotion an outer-frame source already took.
