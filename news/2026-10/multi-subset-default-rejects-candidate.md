# Multi dispatch: a subset-typed default the subset rejects drops the candidate

`multi method right(BB $box = $!content)` (`BB` a subset of `Str`) next to a
plain `multi method right` made `.right` die with
`X::Parameter::Default::TypeCheck` in mutsu: the dispatcher matched the
candidate on a stand-in type object and only the later bind evaluated the
default. Rakudo trial-binds a candidate with a bind check, defaults included,
so a default the subset rejects makes it a non-match and the sibling runs.
Candidate matching now evaluates the default of an unsupplied, subset-typed
parameter and checks it against the subset. Found with CSS::Box (#14159); its
`t/box.t` now gets past construction (the remaining failures are filed
separately).
