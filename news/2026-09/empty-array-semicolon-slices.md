# Parse empty semicolon-list slices in array composers

Array composers now preserve empty semicolon-list slices as `Any` values,
matching Rakudo for forms such as `[;]`, `[;;]`, and `[1;;2]`.
