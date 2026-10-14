# `chars` and `comb($pattern)` on an `is Cool` instance read its user `Str`

Rakudo's `Cool` text methods are `self.Str.METHOD`. `chars` was answered by a pure
cascade arm that stringified the instance itself, and `comb` with a pattern or limit never
reached the shapeless `Cool` step, so a user-defined `Str` was ignored. A user class's
instance now declines the `chars` arm and reaches `answer_shapeless`, and `comb` takes its
pattern and limit there through the user `Str` (#13399).
