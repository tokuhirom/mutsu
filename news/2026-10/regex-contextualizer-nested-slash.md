# Nested slash regexes inside contextualizers

The parser now scans a `$(...)` or `@(...)` contextualizer as code when finding
the end of an enclosing regex literal. A slash-delimited `rx//` in that code no
longer ends the enclosing literal early (#11606). Quoted parentheses inside the
contextualizer also remain part of its code.
