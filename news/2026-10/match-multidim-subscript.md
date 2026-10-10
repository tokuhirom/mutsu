# Match multi-dimensional subscripts walk the capture list

`$/[*;*]` treated the Match as a single scalar and returned it, so
`IO::Maildir`'s `flags` (`set $/[*;*]».Str`) produced `:2,D` instead of `D` and
`move` died in flag dispatch. A Match is now Positional over its captures for
multi-dimensional reads, matching Rakudo. All of `IO::Maildir`'s `t/maildir.t`
now passes.
