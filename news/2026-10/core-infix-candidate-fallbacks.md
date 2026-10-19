User candidates for core infix operators now retain the native operator when
their signatures do not match. Range, mixin, collation, boolean bitwise,
three-way comparison, approximate equality, composition and sequence calls
reuse their existing VM bodies without looking up the rejected user family
again. Integer bitwise and divisibility opcodes also consult user candidates
before coercing their operands. Declared type objects remain complete terms
before an infix, including `min`, `max` and `%%`.
