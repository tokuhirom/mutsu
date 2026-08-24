# `$?FILE` reports an absolute main compilation-unit path

The parser already folded `$?FILE` at compile time, but the main compilation
unit supplied the command-line path verbatim. Invoking a script as
`mutsu relative/path.raku` therefore left `$?FILE` relative, unlike raku's
absolute path.

The runtime now canonicalizes an existing main source path before passing it
to the parser for compile-time `$?FILE` folding. Synthetic program names such
as `-e`, `-`, and `<repl>` remain unchanged because they are not filesystem
paths. The runtime source-file environment, `$*PROGRAM`, and `$*PROGRAM-NAME`
retain the spelling used to invoke the program; this also preserves raku's
relative `callframe.file` and `callframe.gist` output.

The regression coverage checks both direct and interpolated `$?FILE` in a
script invoked through a relative path.
