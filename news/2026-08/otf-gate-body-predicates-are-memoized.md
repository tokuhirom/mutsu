# The OTF gates stop re-walking the routine body

ADR-0019 C6b, following [C6a](routine-identity-fingerprint-is-memoized-on-the-def.md).
Same shape, different category of `FunctionDef.body` reader.

Whether a routine can be compiled on the fly instead of tree-walked is decided by
gates that ask three questions of its body: does it contain a construct the
standalone-compiled form would not preserve (`function_body_needs_interpreter`),
the stricter module/dynamic-single variant of that
(`module_otf_body_needs_interpreter`), and does it declare a `state` variable
(`function_body_declares_state`). Each is a pure predicate that walks the entire
body AST, each was called straight off `def.body`, and the gates run on every
slow-path call to the routine — so the same walks over the same immutable AST were
repeated for the life of the program.

They are now computed once into a `RoutineBodyFacts` memoized on the def
(`FunctionDef::body_facts_cache`, a `OnceLock`, `#[serde(skip)]` like the
fingerprint memo). The three are memoized together rather than separately: the
module-single gate asks for two of them anyway, and one extra walk on first touch
is nothing beside the compile the gates exist to authorize.

`Interpreter::routine_body_facts` is now the *only* place that reads `def.body` for
these facts, which is the point: when the compiler starts supplying them
(the eventual end of C6), one function changes instead of seven call sites.

Separately, the redeclaration path's `is_stub_routine_body(&existing.body)` now
reads the `is_stub` field that C4 already derives at registration.

`FunctionDef.body` readers: 50 -> 47.
