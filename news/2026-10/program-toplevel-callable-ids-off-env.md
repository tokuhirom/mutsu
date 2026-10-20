# A program's top-level routines keep their clone ids off the frame env

Every routine registration mints a clone id under a `__mutsu_callable_id::`
marker. That id scopes `state` variables, targets a non-local `return` and
checks a `wrap` chain. Since #11092, a routine registered directly by a loaded
module's mainline records its id in a per-interpreter table instead of the
importer's frame env, because a mainline runs once (ADR-0084 §2 group 1,
#7817).

The program's own mainline also runs once, but its top-level routines still
put their markers in the frame env: a program with 4000 subs carried 4000
markers in every frame env it copied or captured. `run_unit` now runs the
program's mainline under the same top-level marking. A routine registered
directly at the program's top level keeps its id in the table, while one
declared inside a block or a routine keeps its lexical env marker.
