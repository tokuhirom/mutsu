# The executing round trip carries plain subs and methods and skips source ranges (#15657)

Callgrind of the frontend (profiling build) shows the parser alone at about 96% of the pre-flip
baseline on `Zef/CLI.rakumod` and at the baseline on `grammar-cursor-attributes.t`; the round trip
(spelled parse -> `convert` -> `lower`) is 16.7% and 20.4% of the frontend, which is the whole excess
over the 110% bound. The cut-over removes it; until then two cheap reductions:

- the executing round trip sets a statement's origin to its line only (nothing in the lowering reads
  the source ranges `.AST` reports), and skips the package/type-declaration range bookkeeping;
- a plain named `sub` or `method` (no traits, no `is export`, no return type, no `handles`, parameters
  and body themselves carriable) crosses the round trip as a leaf; `declared_routines` now reads the
  sub names and `&name` parameters inside leaves, so a call such as `c1() += 4` still sees `c1`.

Frontend Ir against the baseline: zef-cli 117.2% -> 116.8%, s12-class-attributes 116.5% -> 114.2%,
grammar-cursor-attributes 122.5% -> 119.3% (the other two cases within 0.2%). #15657 stays open.
