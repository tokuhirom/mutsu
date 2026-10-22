RakuAST regex lowering preserves named captures used as `%` and `%%`
quantifier separators. The source renderer keeps the capture alias adjacent
to `=`, so the existing matcher records each separator capture instead of
treating its name as a `Match` method call. `roast/S05-capture/caps.t` now
passes through the RakuAST frontend.
