# Line numbers count every line break rakudo does

`$?LINE`, error positions and `RakuAST::Origin::Source.original-line` only
counted `\n`, so after a `\r`, VT, FF, U+0085, U+2028 or U+2029 the line number
stayed behind (`\r\n` already counted once through the RakuAST index only).
The parser's line index and the RakuAST origin's line starts now share one
definition (`str_scan::line_break_offsets` / `is_line_break`), counting those
seven characters and `\r\n` as a single break, as rakudo does.
