# Initialize the default IO::Handle path

`IO::Handle.new` and `IO::Handle.bless` now retain the default `IO` type object
as their path. Their gist renders it as `IO::Handle<(IO)>(closed)`, matching
Rakudo, while explicitly supplied paths keep their own representation.
Native handle settings remain visible in `.Capture` after the default path is
declared.
