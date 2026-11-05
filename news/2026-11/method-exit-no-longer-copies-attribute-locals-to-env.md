# A non-pure method exit no longer copies its attribute locals into the env

A compiled method that cannot skip the merge (it writes the env, or takes an
`is rw` parameter) used to copy every attribute-named local (`$!x`, `.x`,
`@!x`, `%!x`) back into the callee env just before the exit work, under the
comment "AssignExpr may have written attribute values to env". Nothing read
those copies afterwards: `reconcile_attrs` recovers a `:=` bound attribute from
the frame's local slot first, the merge excludes attribute-twigil names as
frame state, and the callee env is dropped. An ablation of the loop left all of
`t/` (9265 files) and the roast whitelist green; the only roast failures were
the known environment ones.

On a class with a few attributes, a method that pushes onto an array
attribute, writes a hash attribute and a scalar one, and a method that takes an
`is rw` parameter (50,000 calls each), instructions went from 6,902.8M to
6,851.3M, about 515 per call (second callgrind run of each). The loop was the
last place the fast method path wrote attribute names into the frame env.
