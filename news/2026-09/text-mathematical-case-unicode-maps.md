# Text::MathematicalCase Unicode maps run through mutsu

Unicode `Uni` values now join their numeric codepoints with the default
separator, and `.flat` preserves `Range` values stored as itemized array
elements. Custom `EXPORT::all` stashes also retain callable definitions for
named exported subs. These fixes let the upstream `Text::MathematicalCase`
`t/01-basic.rakutest` pass all 70 assertions under mutsu.
