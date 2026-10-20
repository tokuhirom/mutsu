Proto routine and regex declarations now require a statement separator
before another declaration on the same line, including empty bodies. The
check runs at the body's source boundary, before its AST loses body syntax.
The shared statement guard also recognizes empty ordinary sub bodies. It
preserves routine postfixes and recognizes Unicode line endings without
mistaking embedded-comment text for a separator.

Fatal syntax errors inside proto bodies now remain errors instead of being
discarded by the raw-body recovery path.

The regex engine's proto-dispatch corpus uses explicit declaration separators
so that its syntax and expected output are also valid under Rakudo.
