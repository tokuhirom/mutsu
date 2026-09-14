The RakuAST converter now retains ordinary `@array` interpolation in regex
bodies as `RakuAST::Regex::Interpolation`, including the `sequential` field
after `||`. Matching keeps the existing runtime path and reads the current
array contents at match time.
