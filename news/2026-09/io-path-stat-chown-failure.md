# IO::Path metadata and ownership methods

`IO::Path` now provides `inode`, `dev`, `devtype`, and Unix `chown`. The stat
methods return a Failure for missing paths, and `chown` reports
`X::IO::Chown` through a Failure when the system call fails.

Passing a Failure as a `.chmod` mode now throws its exception. Smartmatching
a Failure against an `IO::Path` also throws the Failure's exception instead
of returning a false match.
