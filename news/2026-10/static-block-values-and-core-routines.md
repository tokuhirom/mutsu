# Block closures, special variables and statement routines for `static`

For `static` 0.0.1 (#15610), measured against rakudo 2026.09:

- `Block` / `PointyBlock` answer `meta-object` (their closure, as
  `compile-time-value`), and a block that names lexicals of the unit being
  compiled can be evaluated before they exist (`-c` runs only the CHECK
  code): the lexicals it uses but does not declare are declared first;
- `$_`, `$/` and `$!` resolve to an implicit special declaration;
- `take`, `emit`, `await` and `return` resolve to routines of the setting.

`static` 0.0.1 `t/02-inference` and `t/15-containers-and-effects` mostly match
(`t/15` 46 -> 2 `not ok`: a negative literal subscript is a soft sorry in
rakudo and still a hard parse error here).
