# A top-level routine called from a `use` block keeps the program's own class

With `my class C { }` in the program and a block `{ use M; callee() }` where `M`
exports a class `C` too, `callee()` (written outside the block) died with
"Undeclared name: C" while the block was open: the frame hid the block's import
of `C` and the type-only key of the program's own `my class` was only consulted
when the bare binding was a scalar or foreign value. The bareword resolution
now treats a binding the frame hides as taken over, so the program's class
answers (ADR-8751, #8751).
