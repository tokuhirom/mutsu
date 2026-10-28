# A one-word quote no longer goes through an identity builtin

`<a>` / `q:w:v/a/` with a single word wrapped the word in `__mutsu_qw_result(x)`, a builtin that
returned its argument. The wrapper is gone: the parser emits the word itself. Deleted: the
parser's `Call` construction and the builtin dispatch arm.
