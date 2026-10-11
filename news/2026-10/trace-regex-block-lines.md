# `use trace` reports the right line for statements in a regex code block

The statements of a regex code block were traced with the line counted from the start of the
block's copied text, so a block written on lines 2-4 reported lines 1-3. A fragment parsed on
the enclosing unit's lent attempt list now adds the lines of the unit that precede its copy,
and `current_line_number` reports the unit's own line, as rakudo does (`rx:i/.../` included).
