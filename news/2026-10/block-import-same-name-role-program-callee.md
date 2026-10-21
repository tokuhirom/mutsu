# A class declared in a routine called from a `use` block composes the program's role

With `my role R { }` in the program and `{ use M; callee() }` where `M` exports
a role `R` too, a `class ... does R` declared inside the top-level `callee`
died with "Invalid typename 'R'": the class header's parent resolution read the
bare binding the frame hides. It now falls back to the program's type-only key
(ADR-8751, #8751).
