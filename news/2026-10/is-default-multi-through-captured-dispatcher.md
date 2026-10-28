# `is default` breaks ties in a captured multi dispatcher

A module's lexical `my proto sub` with `my multi sub` candidates is called through a dispatcher that
captures its candidates as code objects. The dispatchee rows rebuilt from them dropped the
`is default` trait, so a call matching `(Str:D $t)` (default) and `(Str:D $t, *%_)` died with
"Ambiguous call" (`Text::Emoji`'s `to-emoji`). The trait is now carried on the captured candidate.
