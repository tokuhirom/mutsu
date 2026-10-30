# A second importer of an operator-re-exporting slang still sees the operators

A module whose `sub EXPORT` re-exports the operators of a `need`ed module
(`Qwiratry::Query::Slang`) lost them for every importer after the first in a
process: `use-ok 'Qwiratry::Test::Format'; use-ok 'Qwiratry::Test::Location'` died with a
parse error at `$payload ⮷ $location`. The runtime's canonical export scan ran with
the parser's module search paths cleared, so the slang's `need` was unresolved and the
operator-less scan was cached for later parses. The scan now runs under the
interpreter's search paths, which makes all three `Qwiratry::Test` files match `raku`.
