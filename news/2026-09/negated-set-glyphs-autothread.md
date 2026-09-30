# Negated set glyphs autothread over Junctions

The precomposed negated set operators (`∉ ∌ ⊈ ⊉ ⊄ ⊅`) are routines of their own over `Any` in
Rakudo, so a Junction operand autothreads and the negation applies per eigenstate
(`1 ∉ any((1,),(4,))` is `any(False, True)`). mutsu wrapped the positive operator in `!`, which
collapsed the Junction. The parser now emits a distinct `NotThreaded` opcode for the glyph forms,
which negates each eigenstate of the positive operator's (already autothreaded) answer and keeps the
junction kind; the `!` meta form (`!(elem)`, `!⊆`) still collapses, as in Rakudo. Closes #9968.
