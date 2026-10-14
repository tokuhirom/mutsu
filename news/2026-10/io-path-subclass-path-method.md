# `.path` on a user subclass of IO::Path

A user class that inherits from `IO::Path` now answers `.path` with the stored
path text, as `IO::Path` itself does. Before, the accessor was rejected for any
subclass because the declared-class rule only accepts declared public
attributes, so App::RaCoCo's `Fixture::FakeIOPath` (whose `Str` is
`self.path`) died with "No such method 'path'" (part of #13126).
