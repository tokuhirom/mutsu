# Pakku's `add`, `add-serial` and `add-ecosystem` suites pass

All 14 Pakku test files now match rakudo. What the last three needed:

- `RAKULIB` accepts the spec a staging repository prints (`CompUnit::Repository::Staging#name(site)#PATH`,
  or a spelled-out `CompUnit::Repository::Installation#PATH`) and treats it as an installation at `PATH`;
  Pakku hands exactly that to the child that runs a staged dist's tests.
- `CompUnit::Repository::Installation.install` looks through container cells when it reads a META's
  `provides` and `resources`. A META a program assembles (Pakku's recman metas) holds them in cells, and the
  install used to record an empty `provides` and copy no source file.
- An installed META records `ver` next to `version`, as Rakudo does.
- An installation writes a runnable `bin/NAME` launcher (and its `bin/NAME.raku`) for each script, as Rakudo
  does; the script itself stays under its content id where `run-script` finds it.
