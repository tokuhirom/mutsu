# The `core` Installation repository reports the bundled Rakudo-Core distribution

`$*REPO.repo-chain` now holds a `core` `CompUnit::Repository::Installation` between the site
repository and the bundled batteries, and its `.installed` (like
`repository-for-name('core').installed`) returns the bundled Rakudo-Core distribution, as Rakudo's
core repository reports its `rakudo` distribution. Code that walks the Installation repositories
and reads `.installed` (App::ModuleSnap, #9759) no longer sees an empty list. The other bundled
batteries stay FileSystem links, so `mzef install` can still upgrade them.
