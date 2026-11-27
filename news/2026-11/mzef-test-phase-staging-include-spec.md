# `mzef install` runs a dist's own tests against the staged repository again

The default `mzef install <dist>` (tests ON) failed for almost every
distribution whose tests `use` the distribution itself: `Could not find
<Dist> in: (module repositories)` in the Testing phase. zef runs each test
file as `$*EXECUTABLE -I <staging-repo.path-spec> t/foo.t`, and since #14914
the vendored `CompUnit::Repository::Staging` prints rakudo's spelled-out
`path-spec`, `CompUnit::Repository::Staging#name(site)#PATH`. `RAKULIB`
already understood that spelling, but `-I` took the whole string as a
directory name.

`-I` and `RAKULIB` now share one repository-spec normalizer
(`Interpreter::repo_spec`): a spelled-out `Installation`/`Staging` spec is the
installation at `PATH`, a spelled-out `FileSystem` spec the plain path (#19246).
