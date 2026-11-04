# `IO::Spec::Win32.split` of a root and of doubled separators

`IO::Spec::Win32.split('///')` answered a basename of `/`; rakudo answers a
backslash (`IO::Path::Parts.new("","/","\\")`, the example in the
`IO::Spec::Win32` docs). The root cases now follow rakudo: the dirname is the
separator as written (a UNC volume's root is a backslash) and the basename is a
backslash; a dirname loses its trailing separators (`/a//b` splits to `/a`
and `b`); a bare drive (`C:`) splits to `.` and an empty basename. A server
without a share (`//a`) is still treated as a volume by the shared volume
parser; `splitpath`, `canonpath` and `rel2abs` differ from rakudo there too.
