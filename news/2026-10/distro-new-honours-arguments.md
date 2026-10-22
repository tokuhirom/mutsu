# `Distro.new(:name<win32>)` takes its attributes from the arguments

`Distro.new(...)` ignored its arguments and always described the running system, so
RakuPM's `t/known-failure-nojson.t` (which shadows `$*DISTRO` with
`Distro.new(:name('win32'))`) saw Linux and `is-known-windows-test-failure` returned
False. A `Distro` built from arguments now carries exactly those (`name`/`auth`
default to "unknown", the rest are unset, `is-win` follows `mswin32`), as in Rakudo;
the argument-less `Distro.new` still mirrors the process (#14171).
