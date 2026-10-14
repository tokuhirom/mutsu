# Capture and Version resolve Any's reverse, invert and fmt

`Capture.reverse` and `Version.reverse` answer `Any.reverse` (`self.list.reverse`),
`.invert` raises `X::TypeCheck` for non-pair elements as `List.invert` does, and
`.fmt` raises `X::Multi::NoMatch` like Rakudo instead of `X::Method::NotFound`
(#13477).
