# `$*DISTRO` reads `/etc/os-release` like rakudo

On Linux mutsu filled `$*DISTRO` from the kernel (`name` "linux", `release`
"6.18.44-...", `auth` "unknown"), so `$*DISTRO.name eq 'ubuntu'` and
`$*DISTRO ~~ 'ubuntu'` were False. It now follows rakudo's `$*DISTRO`
initialisation, recovered from the shipped setting: `"` is stripped from the
file, `ID` is the name, `HOME_URL` the auth, `VERSION` the version (converted
with `Str.Version`, so `24.04.5 LTS (Noble Numbat)` keeps its words as parts),
`VERSION_ID` the release and `PRETTY_NAME` the description; a missing key
keeps the kernel-derived default. `Distro`/`Raku`/`Compiler` `.gist` also kept
a version's parts instead of its spelling (`24.4.5` for `24.04.5`); it now uses
`Version.Str`. Closes #16572.
