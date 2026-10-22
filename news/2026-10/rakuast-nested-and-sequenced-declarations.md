RakuAST now preserves nested declaring-signature groups as parameter
sub-signatures and comma-linked declarations as one list infix. C-style loop
setup and increment lists keep their written comma structure through the
round-trip, and variable declarations retain `::T` type captures.
