# IO::Spec canonicalizes relative bases and Win32 path parts

`IO::Spec::Unix.rel2abs` and `IO::Spec::Win32.rel2abs` now resolve a relative base against the current directory and canonicalize an already absolute path. Win32 `split`, `splitpath`, and `join` also preserve the expected slash style and empty path parts, including a slash-style UNC volume.
