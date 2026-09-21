Pod::To::Man 1.2.1 now gets the same code-block rendering and subset
metamodel behavior under mutsu as under Rakudo. Multiline concatenations after
`qq{...}` no longer get split at a newline, and `.^refinee` returns the base
type for subset declarations. The distribution reaches 32/35 assertions; the
remaining method, attribute, and subroutine documentation checks are tracked
in #8974 because normal execution still exposes type placeholders instead of
the concrete declarators through Pod's `WHEREFORE`.
