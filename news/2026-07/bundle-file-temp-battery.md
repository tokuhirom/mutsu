# Bundle File::Temp, completing HTTP::UserAgent's dependency layer

`File::Temp` was the one `HTTP::UserAgent` dependency that did not pass its
upstream suite on the first run. The blocker turned out to be an interpreter bug
rather than anything about the library: its `t/03-tempfile` loads the module as

```raku
my (&tempfile, &tempdir) := 'use File::Temp; &tempfile, &tempdir'.EVAL;
```

— deliberately, so the test can install its own `END` phaser *before* File::Temp
installs one. A module `use`d inside a registry-restoring scope lost its own
file-scoped routines when that scope exited, while `loaded_modules` kept claiming
it was loaded, so the returned `&tempfile` died with
`Unknown function: make-temp`. That is fixed separately; with the fix the suite
is 3/3.

So `File::Temp` v0.0.12 (Artistic-2.0) is now vendored into `modules/` alongside
its own dependency `File::Directory::Tree`, and resolves with zero config:

```raku
use File::Temp;
my ($name, $handle) = tempfile;     # removed at exit
my $dir = tempdir;
```

With it, **every runtime dependency of `HTTP::UserAgent` is bundled** — `URI`,
`MIME::Base64`, `HTTP::Status`, `DateTime::Parse`, `Encode`, `File::Temp`,
`File::Directory::Tree`, on top of the already-bundled `IO::Socket::SSL`. The
release-time gate is 53/53 test files across all nine bundled libraries.

`Encode`'s license clarification is still pending upstream
(<https://github.com/sergot/perl6-encode/issues/17>) and remains flagged in the
bundle index and in `docs/batteries/http-deps.md`; nothing else in the layer has
an open question.
