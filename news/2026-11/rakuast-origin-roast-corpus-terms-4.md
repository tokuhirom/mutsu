# `handles q[...]`; a fourth set of roast-corpus origins

A `handles` trait naming its method with a quote construct
(`has A $.bar handles q[aie]`, `Q<b>`, `qq{c}`) was a parse error ("Two
terms in a row"): the trait read `q` as a bare method name. A quote construct
is now read as the term it is.

`.origin` (#15780), from the differential over roast:

- the operand of a subscript is placed from the application's text when the
  parser kept no range for it (`<a b c d>.[*-1]`, `self[0]`,
  `flat(1, 2).hash.<a>`; `subscript_operand_origins`);
- a capture alias with blanks around its `=` (`$<n> = (.*)`);
- a regex declaration's signature visits the implicit invocant and `%_`
  parameters a method's does;
- a `handles` trait on a declaration, its term parsed again from its text;
- a statement-control statement under a phaser (`BEGIN for E.enums { }`).

The enum term shape the sample also showed is filed as #18611.
