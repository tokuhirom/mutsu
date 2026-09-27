# Preserve aliases between hash elements initialized by a bind

mutsu now preserves the shared storage when an indexed assignment is used as
the source of another indexed bind, including scalar variables that autovivify
their hash. This allows `Getopt::Type` 0.2 to pass its complete test suite,
including repeated short and long option aliases.
