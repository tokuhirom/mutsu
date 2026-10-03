# A native return type spelled through a `constant` alias

P5getpwnam picks its passwd struct per OS (`my constant PwStruct =
PwStructLinux`) and declares `sub _getpwuid(uint32 --> PwStruct) is native`.
mutsu followed the alias to decide the return was a CStruct handle, but tagged
the handle with the alias name `PwStruct`. Every method of the real class,
`list` and `scalar` among them, was then missing. The handle is now an
instance of the class the alias names. User::pwent, which builds on
P5getpwnam, reaches parity with rakudo. Its remaining failures fail the same
way under rakudo when the tests run as uid 0.
