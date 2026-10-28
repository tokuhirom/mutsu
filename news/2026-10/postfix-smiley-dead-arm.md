# Dead type-smiley arm removed from the postfix term parser

`postfix_expr_inner` carried a second `:U` / `:D` / `:_` consumer after `primary`, for a type literal
followed by a smiley. `primary` already owns every smiley spelling and no file of `t/` reached the arm,
so it is deleted.
