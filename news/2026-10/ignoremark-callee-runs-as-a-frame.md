# A `:m` rule called as `<r>` runs as a frame over a mark-free subject

A single plain rule declared with `:m` (ignoremark) used to be evaluated eagerly
when called as a subrule, so a code block in it ran once per end computed
instead of once per end entered (`regex r { :m a+ { @log.push: $/.to } }` over
`aax` logged `2,1`; rakudo logs `2`, #7548). When the subject has no combining
marks, `:m` leaves it unchanged, so the call now enters the rule's mark-stripped
program as an ordinary frame, which enters its ends lazily. A subject with
marks still takes the eager path over the stripped view. The match target
records once whether stripping changed its subject (`StrippedMatchTarget::is_unchanged`).
