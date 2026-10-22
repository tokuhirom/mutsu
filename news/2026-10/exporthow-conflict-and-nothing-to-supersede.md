# `X::EXPORTHOW::Conflict` and `X::EXPORTHOW::NothingToSupersede`

A `use`d module whose `EXPORTHOW` declares a built-in declarator again (`DECLARE::class`), supersedes the same
declarator as another imported module, or supersedes a declarator that does not exist now fails the unit with the
typed compile-time exception (`directive` / `declarator` attributes) rakudo throws. `roast/S12-meta/exporthow.t`
reaches 12/12 except where rakudo itself dies. Fixes #14536.
