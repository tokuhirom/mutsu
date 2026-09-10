# A `where` clause may now follow a sub-signature

A Raku parameter can carry a destructuring sub-signature *and* a `where`
post-constraint at once — the sub-signature unpacks the argument, the `where`
tests the argument's own value as `$_`:

```raku
sub f($x ($a, $b) where { $_.elems == 2 }) { ... }
```

mutsu parsed either half alone but not the two together. Each of the four
sub-signature branches in `src/parser/stmt/sub_param/param_inner.rs` — the bare
`(...)`, `$x (...)`, `&cb (...)` and `@a [...]` spellings — consumed the group,
then its `is` traits, and returned. The trailing `where` was left unconsumed, so
the signature parser demanded the closing paren and the declaration failed with
`Confused. expected statement: expected ')'`.

The failure was worse than it looked in real code. A parse error inside a
parameter list backtracks out of the whole enclosing package body, so
`AccessorFacade`'s

```raku
multi trait_mod:<is> (Method $r, :$accessor-facade! (*@a) where { any($_.list) !~~ Code }) is export {
```

surfaced as `X::Undeclared::Symbols: Undeclared routine: AccessorFacade:ver` —
the `module` declarator re-read as a call. An "undeclared routine" naming a
package declarator is a symptom of a parse failure somewhere inside its body,
not of the declarator itself.

The four branches now share one `parse_subsig_tail` helper
(`src/parser/stmt/sub_param/helpers.rs`) that parses everything a parameter may
carry after its sub-signature: `is` traits, a `where` constraint, and a default
value. Sharing it is what keeps the branches in step — three of the four had
already drifted apart on which of the three they accepted, and `$x ($a, $b) =
(1, 2)` (a default after a sub-signature, which rakudo accepts) worked in none
of them.

The constraint is enforced, not merely parsed: a value failing it raises the
same `Constraint type check failed in binding to parameter '$x'; expected
anonymous constraint to be met but got List ((1, 2, 3))` rakudo raises, and a
`multi` candidate carrying one falls through to the next candidate when the
constraint fails.

`t/routines/signature/sub-signature-with-where.t` pins all eight rows of the
issue's matrix — positional and named, slurpy and plain — so the four that
already worked cannot be traded for the four that did not, plus the anonymous
and bracketed spellings, the enforcement and multi-dispatch behavior, and the
trait/default tail.

The named rows are pinned only at the level this change is about — the
signature parses and the call binds — because a separate, pre-existing gap sits
under them: mutsu still reads a `$`-sigil named parameter's sub-signature
(`:$x! ($a, $b)`) as the *rename* form `:min(:$minutes)`, so its inner variables
do not destructure. That is the `$`-sigil twin of the `:@a [...]` bug fixed in
#7758, and is filed as #7865.

Closes #7864.
