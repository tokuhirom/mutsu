# System information objects use their Str methods in string context

Prefix `~`, string comparisons, and interpolation now dispatch the native `Str` methods of `$*KERNEL`, `$*DISTRO`, and `$*VM`. Previously these expressions rendered object placeholders such as `Kernel()` even though an explicit `.Str` returned the system name.
