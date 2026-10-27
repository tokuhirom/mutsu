# `use MONKEY-SEE-NO-EVAL` travels with a closure regex

A regex built in a module that says `use MONKEY-SEE-NO-EVAL` and matched from code that does not
(`my token R { ^ <{$src}> }` returned from the module, `$str ~~ $re` in the caller) died with
`Prohibited regex interpolation`: the pragma was looked up from the matcher's frame instead of the
regex's own scope. The regex now carries the pragma, and the lexicals of its defining frame, so the code
blocks of the interpolated string compile where the regex was written. `Router::Right`'s
`t/04.t` passes (4/4 files, as on Rakudo).
