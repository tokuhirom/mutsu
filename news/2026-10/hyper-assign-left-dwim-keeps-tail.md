# `@a «=« (9, 8)` keeps the left elements the right side does not reach

A hyper assignment whose right operand sets the length (`«=«`, `«+=«`, ...) used to replace the array by the
hyper op's result, so `my @a = 1, 2, 3; @a «=« (9, 8)` left `[9 8]`. It now writes only the elements the
right side reaches, as rakudo does (`[9 8 3]`), and a right side longer than the array wraps onto its slots
without resizing it. The expression still yields the hyper result. Pinned by
`t/lang/operators/hyper-assign-left-dwim.t` and the `dwim left` row of
`t/rakuast/rakuast-stage2-hyper-assign-paths.t`.
