# The `anon` declarator's name parsing converges on the real identifier class

A doc-diff run over `raku-doc/doc/Language/variables.rakudoc` ("The `anon` declarator", ~line 768)
filed two bugs: `say anon class þ {}` / `say anon sub þ { 42 }` died with `comma or statement end
after argument`, and `say anon sub Foo { 42 }` printed `Foo` where Rakudo prints `&Foo`. Both were
already fixed by #6906 before this ticket was picked up — the class expression parser had been
moved off an ASCII-only `c.is_ascii_uppercase() || c == '_'` gate onto the shared
`is_raku_identifier_start` helper, and `say` now routes `Sub`/`Routine` values through native
`.gist` dispatch instead of the string-value fast path, so a named routine renders with its `&`
sigil. Re-verifying the ticket on a current build reproduced neither, and the whole doc snippet
(including the `%operations` example and the `::?CLASS` anon-class example) now matches `raku`
line for line.

Re-verification did surface two genuine residuals in the same family, both fixed here.

**`anon grammar` was never converged.** `anon_grammar_expr` still carried the exact ASCII-only
character class that `anon_class_expr` and `anon_role_expr` had been moved off, so the
expression-position grammar declaration rejected a non-ASCII name that the *statement*-position
declaration accepted. `grammar þ { token TOP { . } }` parsed fine as a statement, but
`(grammar þ { ... }).^name` was a parse error and `anon grammar þ { ... }` silently fell back to
something else entirely (`$g.^name` reported `Str`, and `.parse` then failed with "No such method
'parse' for invocant of type 'Str'"). Pointing that branch at `is_raku_identifier_start` makes all
three expression-position package declarators agree with each other and with the statement path.

**A named `anon class` collided with itself.** `anon` means no symbol is installed, so declaring
`anon class Foo { }` twice in one scope is legal Raku — but mutsu's expression path emitted a plain
`Stmt::ClassDecl`, which claimed `Foo` in the compiler's per-scope `class_names_current_scope` set
and made the second declaration a false `X::Redeclaration: Redeclaration of symbol 'Foo'`. The same
error fired for `class Foo {}; my $a = anon class Foo {};`, where the anon declaration must not
conflict with the real one at all. The fix reuses the channel `anon sub NAME` already uses: the
`"anon"` arm of the expression parser tags the resulting `ClassDecl` with an `__anon_decl` custom
trait (joining `__hoisted` as an established internal marker on that field), and the compiler's
redeclaration check skips a declaration carrying it. That was not a theoretical case — it is what
the new regression test hit on its second `anon class þ`.

Pinned by `t/anon-declarator-name-and-gist.t` (16 assertions, green under both `raku` and mutsu):
ASCII and non-ASCII names for `anon class` / `anon role` / `anon grammar` and for a postfixed
expression-position `grammar`, a non-ASCII-named anon grammar actually parsing input, the anon
class gisting as `(þ)`, and `is_run` guards on the two original doc lines rendering `(þ)` and `&þ`.

The residual — `anon class`/`anon role`/`anon grammar NAME` still *installs* NAME in the namespace,
and two same-named anon classes still share one type object where Rakudo yields two distinct types
— is filed separately as `todo/tickets/anon-package-declarator-still-installs-its-name.md`. It
needs registry-unique internal keying with a user-visible display name, which is a larger slice
than a parse fix.
