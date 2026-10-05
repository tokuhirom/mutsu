# List aggregate methods move to built-in method rows

Any's minmax and sum, and List's permutations and combinations, now dispatch
through the built-in method table. Array inherits the List combinator rows.
Their handlers are shared with the native cascade, preserving numeric
promotion, scalar aggregate behavior, Hash's Pair-list view, lazy combinator
results and fallback behavior for other receiver shapes. Squish stays on its
interpreter path because its identity comparison can call a user WHICH method.
