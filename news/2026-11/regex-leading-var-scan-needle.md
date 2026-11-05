# A regex that begins with `$var` scans for the variable's text

A code-free pattern that begins with a plain `$var` (`$l ~~ / $needle \d+ /`) now
reads the variable once per scan and walks only the start positions where its
`Str` value occurs, as the splice path did before every plain `$var` was read at
the cursor (#16474). Before, the engine was entered at every start position of
the subject and read the variable each time. Patterns with a code block, a
declaration, `:i` or `:ignoremark`, and non-`Str` values keep the per-position read.
