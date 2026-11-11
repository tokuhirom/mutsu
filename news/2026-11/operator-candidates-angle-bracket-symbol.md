# `&infix:«>».candidates` lists the operator's candidates

Rakudo names an operator whose symbol contains `<` or `>` with French quotes
(`infix:«>»`), and mutsu's snapshot of the core candidates keeps that spelling.
The code handle looked itself up as `infix:<>>`, found nothing, and
`.candidates` came back empty for `>`, `<`, `>=`, `<=` and their kin. The
`static` distribution's analyzer dispatches each operator through its
candidates, so it reported every `$x > 0` as a type mismatch (green to partial
in the 2026-11-11 sweep). The lookup now also tries the French-quoted spelling,
and all of static's test files match rakudo again.
