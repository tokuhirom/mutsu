# A `#` comment ending in `?` no longer breaks a `rule` body

Sigspace insertion in `rule` bodies read the text of a just-copied line comment as pattern, so a comment ending in `?`, `*` or `+` (`# maybe this could be const/final?`) made the previous term look quantified. The pass then marked it backtrackable and dropped the comment's newline, which turned the following atoms into comment text. C::Parser's `translation-unit` rule hit this, so none of its grammar tests could parse anything; all nine of its test files now pass.
