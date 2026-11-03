# `mzef install` completes through Zef's Staging fallback

`mzef install --/test Method::Also` used to stop with `Could not find
.../modules/Rakudo-Core/0` ([#16149](https://github.com/tokuhirom/mutsu/issues/16149)).
It now installs the distribution into the site repository, and the installed
module loads. Four independent gaps sat on the path of
`Zef::Client.make-install`'s `CompUnit::Repository::Staging` fallback:

- The core repository's distribution listed `provides` as plain `Name => path`
  strings, so zef's `.provides<Name>.keys.head` was the string's own key `0`. It
  now has an installed distribution's shape, `{ path => { file => .. } }`, and
  `content($path)` points at a file that exists.
- `$*REPO.load($absolute-path)` refused a file outside the repository's prefix.
  Rakudo loads it, and a class declared inside an existing package
  (`class CompUnit::Repository::Staging`) becomes reachable by name afterwards,
  while a new top-level package stays out of GLOBAL.
- `{ level => 1, message => "x{$_}" }` was a Block: a `$_` inside a string's
  interpolation block was counted as the body's own topic. It is a Hash, as in
  rakudo, so zef's logger messages got their text instead of `Nil`.
- `CompUnit::Repository::Installation.install` returned `False` for a
  distribution that reached it through a container (zef's `$candidate.dist`);
  it now looks through the container (and a role mixin).

Regression tests: `t/modules/compunit/core-distribution-provides-installed-form.t`,
`compunit-repository-load-absolute-path.t`,
`compunit-repository-staging-install-wrapped-dist.t` and
`t/collections/hash/hash-composer-interpolated-topic-in-string.t`.
