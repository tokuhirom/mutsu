# Join lazy collections without pulling them

`join` now renders a lazy collection as `...` without evaluating its elements.
This applies to method and routine calls, infinite ranges, closure sequences,
arrays backed by lazy sequences, and explicitly lazy finite lists.
