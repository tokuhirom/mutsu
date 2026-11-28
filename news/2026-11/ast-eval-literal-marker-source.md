`Str.AST` and string EVAL now parse the exposed codepoints of a source string.
A literal U+10FFFD is therefore escaped only once, so AST EVAL preserves its
grapheme and distinguishes it from a utf8-c8 synthetic invalid byte.
