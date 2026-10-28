# `has T $.x handles(@names)` takes a variable inside the parentheses

`handles @names` was understood, but the parenthesised spelling accepted only names, pairs and word lists, so `has HTML::Canvas::Path2D $.path is graphics handles(@PathAPI) .= new: ...` was a parse error ("Two terms in a row") and HTML::Canvas could not load. A `$`/`@`/`%` variable is a handle spec inside the parentheses too. The bare `handles @names = Class.new` also keeps its initializer now (the variable was parsed as an assignment expression and swallowed the `= Class.new`).
