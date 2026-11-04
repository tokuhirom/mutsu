# The bare `::` stash lists what a `sub EXPORT` hash installed

`::{"X::Errno::$key"}`, `::{'Pkg::Name'}:exists` and `::.keys` read the importing scope's pad
through a runtime scan of the environment, which never held the keys a module's `sub EXPORT` hash
installed (`%e{"X::Th::A"} = ...`); `MY::` listed them because it also reads the scope's import
aliases. The bare `::` stash now adds the same import aliases. Universal::Errno's
`t/constants.t6` goes from 0 to 134 of 134 assertions, as under rakudo (#16555).
