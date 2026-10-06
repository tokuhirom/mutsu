A `Supplier.quit` now reaches taps on derived supplies such as `map`, `grep`,
`head`, and `tail`, including multi-stage chains. A `tail` drops its buffered
values when the source quits.
