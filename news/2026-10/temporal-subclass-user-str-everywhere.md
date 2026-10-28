# A user `Str` on a Date/DateTime subclass reaches interpolation, `.gist` and `.Stringy`

For `class P is Date { method Str { "custom" } }`, `~$p` already called the user `Str`, but
`"$p"`, `$p.gist`, `$p.Stringy` and `[$p].gist` still printed the built-in date. `Date.gist` and
`Mu.Stringy` are `self.Str` in rakudo. `dispatch_temporal_method` now answers `gist`/`Stringy` of a
subclass that declares `Str` through it, and `Mu.Stringy` calls `Str` through the compiled method
path (the plain `call_method_with_values` reached the built-in ancestor's native `Str` first).
Covered by `t/types/temporal/temporal-subclass-user-str-in-all-contexts.t`.
