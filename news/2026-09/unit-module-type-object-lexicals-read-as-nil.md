# A `unit module` scalar holding a type object no longer reads as `Nil`

Reported on #10379: in a `unit module`, a file-scope scalar that holds a type
object (`my IO::Handle $fh;`, `my Int $x;`, an untyped `my $x;`, or
`my $t = Int;`) read back as `Nil` from a plain (non-`our`) routine of the
module, where Rakudo gives `IO::Handle`, `Int` and `Any`. The difference was not
only cosmetic: `$fh ~~ IO::Handle` was `False` and `.WHAT` was `Nil`.

The module loader removes from the importer's `env` any bare key whose value is
a type object owned by another package, which is how a leaked type name such as
`Inner` looks. A scalar `my $x` is stored under the sigil-less key `x`, so a
variable holding a type object matched that rule and was removed too. The loader
then moved the module's file-scope lexicals into its own store by reading those
same keys, found nothing for `x`, and seeded the variable with `Nil`. Defined
values (`5`, `"s"`) are not type objects and were never removed, which is why
only reads of a declaration-time default appeared to be affected.

The cleanup now skips the names that the `unit_lexicals` extraction takes over,
so those variables are no longer mistaken for type names.

`t/modules/module-file-scope-type-object-lexical.t` pins the reported shape and
its variants (typed, untyped, assigned type object, list declaration), and that
the braced `module M { ... }` form neither regresses nor leaks its lexicals.
