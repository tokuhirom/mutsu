# Cygwin specs recognize Win32 drive paths as absolute

`IO::Spec::Cygwin.is-absolute` now recognizes both POSIX paths and Win32-style
drive paths such as `C:\\foo`.
